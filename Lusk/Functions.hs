module Lusk.Functions where

import Lusk.Value

lPrint :: HaskellIOFunction
lPrint [] = return Nil
lPrint (a:args) = do
  putStrLn $ show a
  lPrint args