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
    ("print", HIOFunction lPrint)
  ]

-- lua considers only "nil" and "false" as false values
-- everything else is true
toBool :: Value -> Bool
toBool Nil = False
toBool (Bool b) = b
toBool v = True

-- map operators to haskell funcions
evalUnaryOp :: OpType -> Value -> Value
evalUnaryOp Sub = negate
evalUnaryOp Not = \a -> Bool $ not (toBool a)

evalOp :: OpType -> Value -> Value -> Value
evalOp Add a b = (+) a b
evalOp Sub a b = (-) a b
evalOp Mul a b = (*) a b
evalOp Div a b = (/) a b
evalOp Pow a b = (**) a b
evalOp Lt a b = Bool $ (<) a b
evalOp Gt a b = Bool $ (>) a b
evalOp LtEq a b = Bool $ (<=) a b
evalOp GtEq a b = Bool $ (>=) a b

evalList :: (Monad m) => [SyntaxTree] -> [Value] -> StateT SymbolTable (ErrorT String m) [Value]
evalList (v:vs) vs' = do
  v' <- eval v
  case vs of
    [] -> return (reverse $ v':vs')
    _ -> evalList vs (v':vs')

-- main evaluation function
eval :: (Monad m) => SyntaxTree -> StateT SymbolTable (ErrorT String m) Value
eval (NumberLiteral n) = return $ Number n
eval (StringLiteral str) = return $ String str
eval (BoolLiteral b) = return $ Bool b
eval (Parentheses p) = eval p
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
eval (Assignment ns vs) = do
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
    (HFunction f) -> return $ f args'
    (HIOFunction f) -> liftIO $ f args'
    _ -> return Nil

eval (Chunk (s:stats)) = do
  res <- eval s
  case stats of
    [] -> return res
    _ -> eval $ Chunk stats