module Lusk.Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import qualified Data.Map as M

data OpType 
  = Add 
  | Sub 
  | Mul 
  | Div 
  | Pow
  | Lt 
  | Gt 
  | LtEq 
  | GtEq
  | Eq
  | NotEq
  | And
  | Or
  | Not deriving (Enum, Eq, Ord, Show)

data SyntaxTree
  = Empty
  | Var String
  | NumberLiteral Double
  | StringLiteral [Char]
  | BoolLiteral Bool
  | Parentheses SyntaxTree
  | UnaryOp OpType SyntaxTree
  | BinaryOp OpType (SyntaxTree, SyntaxTree)
  | Call SyntaxTree [SyntaxTree]
  | Assignment [SyntaxTree] [SyntaxTree]
  | IfStat SyntaxTree SyntaxTree SyntaxTree
  | Chunk [SyntaxTree] deriving (Eq, Show)


-- parsing

getBoolValue :: [Char] -> Bool
getBoolValue "true" = True
getBoolValue "false" = False

getOpType :: [Char] -> OpType
getOpType "+" = Add
getOpType "-" = Sub
getOpType "*" = Mul
getOpType "/" = Div
getOpType "<" = Lt
getOpType ">" = Gt
getOpType "<=" = LtEq
getOpType ">=" = GtEq
getOpType "==" = Eq
getOpType "~=" = NotEq
getOpType "and" = And
getOpType "or" = Or
getOpType "not" = Not

precedence :: M.Map OpType Integer
precedence = M.fromList [
    (Or, 40),
    (And, 50),
    (Eq, 80),
    (NotEq, 80),
    (LtEq, 90),
    (GtEq, 90),
    (Lt, 90),
    (Gt, 90),
    (Add, 100),
    (Sub, 100),
    (Mul, 110),
    (Div, 110),
    (Pow, 120)
  ]

rightAssociative :: OpType -> Bool
rightAssociative Pow = True
rightAssociative op = False

-- helper for operator associativity and precedence
binary :: OpType -> (SyntaxTree, SyntaxTree) -> SyntaxTree
binary op (l, BinaryOp r (rl, rr)) =
  case rightAssociative op of
    True -> if r == op then normal else invert
    False -> invertWhenNecessary
  where 
    invert = BinaryOp r (BinaryOp op (l, rl), rr)
    normal = BinaryOp op (l, BinaryOp r (rl, rr))
    invertWhenNecessary = if M.lookup r precedence <= M.lookup op precedence then invert else normal

binary op t = BinaryOp op t


-- identifier := [a-z][a-z0-9-_]*
parseIdentifier :: Parser SyntaxTree
parseIdentifier = do
  first <- letter
  rest <- many (letter <|> digit <|> oneOf "-_")

  -- add first to the rest
  return $ Var (first:rest)

-- real := [0-9]+.[0-9]+
parseRealNumber :: Parser SyntaxTree
parseRealNumber = do
  try $ do
    first <- (many1 digit)
    char '.'
    frac <- (many1 digit)
    return $ NumberLiteral (read (first ++ "." ++ frac))
  <|> do
    char '.'
    frac <- (many1 digit)
    return $ NumberLiteral (read ("0" ++ "." ++ frac))

-- number := [0-9]+
parseNumber :: Parser SyntaxTree
parseNumber = do
  try parseRealNumber
  <|> (liftM (NumberLiteral . read) $ many1 digit)

-- bool := true|false
parseBool = do
  bool <- (string "true") <|> (string "false")
  return $ BoolLiteral (getBoolValue bool)

-- string := ("|') (.*) ("|')
parseString :: Parser SyntaxTree
parseString = do
  oneOf "\"'"
  str <- many (noneOf "\"'")
  oneOf "\"'"
  return $ StringLiteral str

-- parentheses := '(' expr ')'
parseParentheses :: Parser SyntaxTree
parseParentheses = do
  char '('
  spaces
  expr <- parseExpr
  spaces
  char ')'
  return $ Parentheses expr

-- primary := number | string | symbol | parentheses
parsePrimaryExpr :: Parser SyntaxTree
parsePrimaryExpr = do
  try parseNumber
  <|> try parseBool
  <|> try parseString 
  <|> try parseIdentifier
  <|> parseParentheses

-- unary := '-' primaryExpr | primaryExpr
parseUnaryExpr :: Parser SyntaxTree
parseUnaryExpr = do
  try $ do
    op <- (string "-" <|> string "not")
    spaces
    rhs <- case op of
      "not" -> parseExpr
      _ -> parsePrimaryExpr
    return $ UnaryOp (getOpType op) rhs
  <|> parsePrimaryExpr

-- call := unary '(' exprList ')' | unary
parseCallExpr :: Parser SyntaxTree
parseCallExpr = do
  unary <- parseUnaryExpr
  callHelper unary
    where 
      callHelper :: SyntaxTree -> Parser SyntaxTree
      callHelper lhs = do
        try $ do
          spaces >> char '('
          args <- parseExprList
          spaces >> char ')'
          callHelper $ Call lhs args
        <|> return lhs

-- pow := call '^' expr | call
parsePowExpr :: Parser SyntaxTree
parsePowExpr = do
  try $ do
    lhs <- parseCallExpr
    spaces
    char '^'
    spaces
    rhs <- parseExpr
    return $ binary Pow (lhs, rhs)
  <|> parseCallExpr

-- multiplicative := unary (*|/) expr | unaryExpr
parseMultiplicativeExpr :: Parser SyntaxTree
parseMultiplicativeExpr = do
  try $ do 
    lhs <- parsePowExpr
    spaces
    op <- oneOf "*/"
    spaces
    rhs <- parseExpr
    return $ binary (getOpType [op]) (lhs, rhs)
  <|> parsePowExpr

-- additive := multiplicative (+|-) expr | multiplicative
parseAdditiveExpr :: Parser SyntaxTree
parseAdditiveExpr = do
  try $ do
    lhs <- parseMultiplicativeExpr
    spaces
    op <- oneOf "+-"
    spaces
    rhs <- parseExpr
    return $ binary (getOpType [op]) (lhs, rhs)
  <|> parseMultiplicativeExpr

-- relational := additive (<|>|<=|>=) expr | additive
parseRelationalExpr :: Parser SyntaxTree
parseRelationalExpr = do
  try $ do
    lhs <- parseAdditiveExpr
    spaces
    op <- string "<" <|> string ">" <|> string "<=" <|> string ">="
    spaces
    rhs <- parseExpr
    return $ binary (getOpType op) (lhs, rhs)
  <|> parseAdditiveExpr

-- logic := relational (and|or) expr | relational
parseLogicExpr :: Parser SyntaxTree
parseLogicExpr = do
  try $ do
    lhs <- parseRelationalExpr
    spaces
    op <- string "and" <|> string "or"
    spaces
    rhs <- parseExpr
    return $ binary (getOpType op) (lhs, rhs)
  <|> parseRelationalExpr

-- equality := logic (==|~=) expr | logic
parseEqualityExpr :: Parser SyntaxTree
parseEqualityExpr = do
  try $ do
    lhs <- parseLogicExpr
    spaces
    op <- string "==" <|> string "~="
    spaces
    rhs <- parseExpr
    return $ binary (getOpType op) (lhs, rhs)
  <|> parseLogicExpr

-- expr := assignment
parseExpr :: Parser SyntaxTree
parseExpr = parseEqualityExpr

-- assignment := identifier '=' expr | equality
parseAssignment :: Parser SyntaxTree
parseAssignment = do
  try $ do
    names <- parseNameList
    spaces
    op <- char '='
    noneOf "="
    spaces
    exprs <- parseExprList
    return $ Assignment names exprs
  <|> parseExpr

-- nameList := identifier (, nameList*)
parseNameList :: Parser [SyntaxTree]
parseNameList = 
  parseIdentifier `sepBy` (char ',' >> spaces)

-- exprList := expr (, exprList*)
parseExprList :: Parser [SyntaxTree]
parseExprList = 
  parseExpr `sepBy` (spaces >> char ',' >> spaces)

-- ifStat := 'if' expr 'then' stat+ 'end'
parseIfStat :: Parser SyntaxTree
parseIfStat = do
  string "if" >> notFollowedBy alphaNum
  elseIfHelper
  where
    elseIfHelper = do
      cond <- parseExpr
      string "then" >> notFollowedBy alphaNum
      block <- parseStat <* (string "end" <|> string "elseif" <|> string "else")
      case end of
        "elseif" -> do
          right <- elseIfHelper
          return $ IfStat cond block right
        "else" -> do
          right <- parseChunk
          string "end"
          return $ IfStat cond block right
        "end" -> do
          return $ IfStat cond block Empty

-- stat := expr
parseStat :: Parser SyntaxTree
parseStat = do
  try parseIfStat
  <|> try parseAssignment
  <|> parseExpr

-- chunk := stat+
parseChunk :: Parser SyntaxTree
parseChunk = do
  parseStat `sepBy` spaces >>= \s -> return $ Chunk s

readExpr :: String -> Either String SyntaxTree
readExpr input = case parse parseChunk "says" input of
  Left err -> Left ("error: " ++ show err)
  Right expr -> Right expr