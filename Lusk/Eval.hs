module Lusk.Eval where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M
import Lusk.Parser -- for eval patterns
import Lusk.Functions
import Lusk.Value

type SymbolTable = M.Map String Value

-- The main state of the interpreter
data LuskState = LuskState {

  -- Local symbol table
  symbolTable :: SymbolTable,

  -- The return value in a function call
  retVal :: Value,

  -- True after a return statement
  ret :: Bool,

  -- True after a break statement
  brk :: Bool,

  -- True after a continue statement
  cont :: Bool,

  -- Parent state
  parent :: Maybe LuskState
}

globalTable :: SymbolTable
globalTable = 
  M.fromList [
    ("pi", Number pi),
    ("print", HIOFun lPrint),
    ("type", HFun lType)
  ]

globalState :: LuskState
globalState = LuskState {
  symbolTable = globalTable,
  retVal = Nil,
  ret = False,
  brk = False,
  cont = False,
  parent = Nothing
}

-- State Monad
type SM m a = StateT LuskState (ErrorT String m) a

modifyST :: LuskState -> SymbolTable -> LuskState
modifyST s st = s { symbolTable = st }

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
evalOp Concat a b = String $ (++) (show a) (show b)
evalOp Lt a b = Boolean $ (<) a b
evalOp Gt a b = Boolean $ (>) a b
evalOp LtEq a b = Boolean $ (<=) a b
evalOp GtEq a b = Boolean $ (>=) a b
evalOp Eq a b = Boolean $ (==) a b
evalOp NotEq a b = Boolean $ not ((==) a b)

evalList :: (Monad m) => (MonadIO m) => [SyntaxTree] -> [Value] -> SM m [Value]
evalList [] vs' = return $ reverse vs'
evalList (v:vs) vs' = do
  v' <- eval v
  evalList vs (v':vs')

-- main evaluation function
eval :: (Monad m) => (MonadIO m) => SyntaxTree -> SM m Value
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
      doMultipleAssign [] = return Nil
      doMultipleAssign (p:ps) = do
        let (name, value) = p
        s <- get
        let t = symbolTable s
        put (modifyST s (M.insert name value t))
        doMultipleAssign ps

eval (Var s) = do
  st <- get
  getVar s st
  where
    getVar s st = do
      let t = symbolTable st
      case M.lookup s t of
        Just v -> return v
        Nothing -> do
          let p = parent st
          case p of
            Just prnt -> do
              getVar s prnt
            Nothing -> return Nil

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

-- Table subscript
eval (Subscript t k) = do
  t' <- eval t
  k' <- eval k
  extract t' k'
  where
    -- Extract a value from the table with the given [key]
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

eval (WhileStat c b) = do
  c' <- eval c
  case toBool c' of
    True -> do
      eval b
      eval $ WhileStat c b
    False -> return Nil

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