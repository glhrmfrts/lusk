module Lusk.Eval where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M
import Lusk.Parser -- for eval patterns
import Lusk.Functions
import Lusk.Value

type SymbolTable = M.Map String Value

symbolTable :: SymbolTable
symbolTable = 
  M.fromList [
    ("pi", Number pi),
    ("print", HIOFun lPrint),
    ("type", HFun lType)
  ]

-- lua considers only "nil" and "false" as false values
-- everything else is true
toBool :: Value -> Bool
toBool Nil = False
toBool (Boolean b) = b
toBool v = True

-- map operators to haskell funcions
evalUnaryOp :: OpType -> Value -> Value
evalUnaryOp Sub = negate
evalUnaryOp Not = \a -> Boolean $ not (toBool a)

evalOp :: OpType -> Value -> Value -> Value
evalOp Add a b = (+) a b
evalOp Sub a b = (-) a b
evalOp Mul a b = (*) a b
evalOp Div a b = (/) a b
evalOp Pow a b = (**) a b
evalOp Lt a b = Boolean $ (<) a b
evalOp Gt a b = Boolean $ (>) a b
evalOp LtEq a b = Boolean $ (<=) a b
evalOp GtEq a b = Boolean $ (>=) a b
evalOp Eq a b = Boolean $ (==) a b
evalOp NotEq a b = Boolean $ not ((==) a b)

evalList :: (Monad m) => (MonadIO m) => [SyntaxTree] -> [Value] -> StateT SymbolTable (ErrorT String m) [Value]
evalList [] vs' = return $ reverse vs'
evalList (v:vs) vs' = do
  v' <- eval v
  evalList vs (v':vs')

-- main evaluation function
eval :: (Monad m) => (MonadIO m) => SyntaxTree -> StateT SymbolTable (ErrorT String m) Value
eval (Empty) = return Nil
eval (NumberLit n) = return $ Number n
eval (StringLit str) = return $ String str
eval (BoolLit b) = return $ Boolean b
eval (TableLit vs) = do
  vs' <- evalList vs []
  return $ Table (zip [Number (fromIntegral x) | x <- [1..length vs]] vs')
eval (Paren p) = eval p
eval (UnaryOp op r) = 
  eval r >>= \v -> return (evalUnaryOp op $ v)
eval (BinaryOp op (l, r)) =
  case op of
    And ->
      eval l >>= \l' ->
        if toBool l' then
          eval r >>= \r' -> return r'
        else
          return l'
    Or ->
      eval l >>= \l' -> 
        if not (toBool l') then
          eval r >>= \r' -> return r'
        else
          return l'
    _ ->
      eval l >>= \l' -> eval r >>= \r' -> return (evalOp op l' r')

-- takes a list of names and a list of values
-- evaluates all the values first (in case of 'variable swaping')
-- then zip that into a list of pairs and add to a symbol table
eval (Assign ns vs) = do
  vs' <- evalList vs []
  doMultipleAssign $ zip (Prelude.map (\(Var s) -> s) ns) vs'
    where
      doMultipleAssign :: (Monad m) => [(String, Value)] -> StateT SymbolTable (ErrorT String m) Value
      doMultipleAssign [] = return Nil
      doMultipleAssign (p:ps) = do
        let (name, value) = p
        modify (M.insert name value)
        doMultipleAssign ps

eval (Var s) = do
  t <- get
  case M.lookup s t of
    Nothing -> return Nil
    Just v -> return v

eval (Call fn args) = do
  fn' <- eval fn
  args' <- evalList args []
  case fn' of
    (HFun f) -> callHFun $ f args'
    (HIOFun f) -> callHIOFun $ f args'
    _ -> return Nil
  where
    callHFun r = do 
      case r of
        Left err -> throwError err
        Right val -> return val
    callHIOFun f = do
      r <- liftIO f
      case r of
        Left err -> throwError err
        Right val -> return val

-- | Table subscript
eval (Subscript t k) = do
  t' <- eval t
  k' <- eval k
  extract t' k'
  where
    -- | Extract a value from the table with the given [key]
    extract (Table pairs) key = do
      let found = (filter (\(tk, tv) -> tk == key) pairs)
      if length found > 0 then
        return $ snd $ head found
      else
        return Nil

eval (IfStat c t r) = do
  c' <- eval c
  if toBool c' then eval t else eval r
  return Nil

eval (Block (s:stats)) = do
  eval s
  case stats of
    [] -> return Nil
    _ -> eval $ Block stats

eval (Chunk (s:stats)) = do
  res <- eval s
  case stats of
    [] -> return res
    _ -> eval $ Chunk stats