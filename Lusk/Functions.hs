module Lusk.Functions where

-- This module contains all of the implementation-defined global functions

import Lusk.Value


lPrint :: HaskellIOFun
lPrint [] = return $ Right Nil
lPrint (a:args) = do
  putStrLn $ show a
  lPrint args

lType :: HaskellFun
lType [] = Left "\"type\" requires 1 argument"
lType [Nil] = Right $ String "nil"
lType [Number n] = Right $ String "number"
lType [Boolean b] = Right $ String "boolean"
lType [String s] = Right $ String "string"
lType [Table _] = Right $ String "table"
lType [HFun _] = Right $ String "function"
lType vs = lType []