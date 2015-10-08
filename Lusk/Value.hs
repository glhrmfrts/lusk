module Lusk.Value where

import Data.List

type HaskellFun = ([Value] -> (Either String Value))
type HaskellIOFun = ([Value] -> IO (Either String Value))

data Value
  = Nil
  | Number Double
  | Boolean Bool
  | String [Char] 
  | Table [(Value, Value)]
  | HFun HaskellFun
  | HIOFun HaskellIOFun

instance Num Value where
  (+) (Number a) (Number b) = Number (a + b)
  (+) (Number a) (String b) = String (show a ++ b)
  (+) (String a) (Number b) = String (a ++ show b)
  (+) (String a) (String b) = String (a ++ b)
  (-) (Number a) (Number b) = Number (a - b)
  (*) (Number a) (Number b) = Number (a * b)
  fromInteger a = Number (fromIntegral a)

instance Fractional Value where
  (/) (Number a) (Number b) = Number (a / b)

instance Floating Value where
  (**) (Number a) (Number b) = Number (a ** b)

instance Eq Value where
  (==) (Number a) (Number b) = (a == b)
  (==) (Number a) _ = False
  (==) (String a) (String b) = (a == b)
  (==) (String a) _ = False
  (==) (Boolean a) (Boolean b) = (a == b)
  (==) (Boolean a) _ = False
  (==) (Table a) _ = False
  (==) (HFun a) _ = False
  (==) (Nil) (Nil) = True
  (==) (Nil) _ = False

instance Ord Value where
  (<) (Number a) (Number b) = (a < b)
  (>) (Number a) (Number b) = (a > b)
  (<=) (Number a) (Number b) = (a <= b)
  (>=) (Number a) (Number b) = (a >= b)

instance Show Value where
  show (Nil) = "nil"
  show (Number a) = show a
  show (Boolean a) = if a then "true" else "false"
  show (String a) = a
  show (Table a) = "{" ++ intercalate "," [show (fst x) ++ "=" ++ show (snd x) | x <- a] ++ "}"