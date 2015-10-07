module Lusk.Value where

type HaskellFunction = ([Value] -> Value)
type HaskellIOFunction = ([Value] -> IO Value)

data Value
  = Nil
  | Number Double
  | Bool Bool
  | String [Char] 
  | Table [(Value, Value)]
  | HFunction HaskellFunction
  | HIOFunction HaskellIOFunction

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
  (==) (Bool a) (Bool b) = (a == b)
  (==) (Bool a) _ = False
  (==) (Table a) _ = False
  (==) (HFunction a) _ = False
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
  show (Bool a) = if a then "true" else "false"
  show (String a) = a