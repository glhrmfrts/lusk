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
  | Concat
  | Eq
  | NotEq
  | And
  | Or
  | Not deriving (Enum, Eq, Ord, Show)

data SyntaxTree
  = Empty
  | Var String -- Name of the variable
  | NumberLit Double
  | StringLit String
  | BoolLit Bool
  | TableLit [SyntaxTree] -- Table items
  | Paren SyntaxTree
  | UnaryOp OpType SyntaxTree
  | BinaryOp OpType (SyntaxTree, SyntaxTree) -- Operator type, (left and right) expressions
  | Call SyntaxTree [SyntaxTree] -- Function and arguments
  | Subscript SyntaxTree SyntaxTree -- Table and key
  | Assign [SyntaxTree] [SyntaxTree] -- Names and values
  | IfStat SyntaxTree SyntaxTree SyntaxTree -- Condition, then-block, else|elseif-block
  | WhileStat SyntaxTree SyntaxTree -- Condition, then-block
  | Block [SyntaxTree] -- Statements
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
getOpType ".." = Concat
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
    (Concat, 95),
    (Add, 100),
    (Sub, 100),
    (Mul, 110),
    (Div, 110),
    (Pow, 120)
  ]

rightAssociative :: OpType -> Bool
rightAssociative Pow = True
rightAssociative op = False

-- | Helper for operator associativity and precedence
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

reserved :: String -> Parser String
reserved s = do
  w <- string s
  notFollowedBy alphaNum
  return w

-- | Tokens that mean the end of a block
terminators :: Parser String
terminators = do
  try (reserved "end")
  <|> try (reserved "elseif")
  <|> (reserved "else")

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
    return $ NumberLit (read (first ++ "." ++ frac))
  <|> do
    char '.'
    frac <- (many1 digit)
    return $ NumberLit (read ("0" ++ "." ++ frac))

-- number := [0-9]+
parseNumber :: Parser SyntaxTree
parseNumber = do
  try parseRealNumber
  <|> (liftM (NumberLit . read) $ many1 digit)

-- bool := true|false
parseBool = do
  bool <- (string "true") <|> (string "false")
  return $ BoolLit (getBoolValue bool)

-- string := ("|') (.*) ("|')
parseString :: Parser SyntaxTree
parseString = do
  oneOf "\"'"
  str <- many (noneOf "\"'")
  oneOf "\"'"
  return $ StringLit str

-- parentheses := '(' expr ')'
parseParen :: Parser SyntaxTree
parseParen = do
  char '('
  spaces
  expr <- parseExpr
  spaces
  char ')'
  return $ Paren expr

-- tableItems := primary '=' expr | expr
parseTableItems :: Parser [SyntaxTree]
parseTableItems = parseExprList

-- table := '{' expr* '}'
parseTable :: Parser SyntaxTree
parseTable = do 
  char '{'
  spaces
  items <- parseTableItems
  spaces
  char '}'
  return $ TableLit items

-- primary := number | string | table | symbol | parentheses
parsePrimaryExpr :: Parser SyntaxTree
parsePrimaryExpr = do
  try parseNumber
  <|> try parseBool
  <|> try parseString
  <|> try parseTable
  <|> try parseIdentifier
  <|> parseParen

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
          args <- (try $ parseExprList <|> (do { return [] }))
          spaces >> char ')'
          callHelper $ Call lhs args
        <|> return lhs

-- sub := call '[' expr ']' | call
parseSubExpr :: Parser SyntaxTree
parseSubExpr = do
  call <- parseCallExpr
  subHelper call
    where
      subHelper :: SyntaxTree -> Parser SyntaxTree
      subHelper lhs = do
        try $ do
          spaces >> char '['
          i <- parseExpr
          spaces >> char ']'
          subHelper $ Subscript lhs i
        <|> return lhs

-- pow := call '^' expr | call
parsePowExpr :: Parser SyntaxTree
parsePowExpr = do
  try $ do
    lhs <- parseSubExpr
    spaces
    char '^'
    spaces
    rhs <- parseExpr
    return $ binary Pow (lhs, rhs)
  <|> parseSubExpr

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

-- concat := additive '..' expr | additive
parseConcatExpr :: Parser SyntaxTree
parseConcatExpr = do
  try $ do
    lhs <- parseAdditiveExpr
    spaces
    op <- string ".."
    spaces
    rhs <- parseExpr
    return $ binary (getOpType op) (lhs, rhs)
  <|> parseAdditiveExpr

-- relational := concat (<|>|<=|>=) expr | concat
parseRelationalExpr :: Parser SyntaxTree
parseRelationalExpr = do
  try $ do
    lhs <- parseConcatExpr
    spaces
    op <- string "<" <|> string ">" <|> string "<=" <|> string ">="
    spaces
    rhs <- parseExpr
    return $ binary (getOpType op) (lhs, rhs)
  <|> parseConcatExpr

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
parseAssign :: Parser SyntaxTree
parseAssign = do
  try $ do
    names <- parseNameList
    spaces
    op <- char '='
    noneOf "="
    spaces
    exprs <- parseExprList
    return $ Assign names exprs
  <|> parseExpr

-- nameList := identifier (, nameList*)
parseNameList :: Parser [SyntaxTree]
parseNameList = 
  parseIdentifier `sepBy` (char ',' >> spaces)

-- exprList := expr (, exprList*)
parseExprList :: Parser [SyntaxTree]
parseExprList = do
  e <- parseExpr
  listHelper [e]
  where
    listHelper ps = do
      try $ do
        spaces
        char ','
        spaces
        expr <- parseExpr
        listHelper $ expr:ps
      <|> (return $ reverse ps)

-- ifStat := 'if' expr 'then' stat+ 'end'
parseIfStat :: Parser SyntaxTree
parseIfStat = do
  reserved "if"
  elseIfHelper
  where
    elseIfHelper = do
      spaces
      cond <- parseExpr
      spaces
      reserved "then"
      spaces
      b <- manyTill parseStat (lookAhead $ spaces >> terminators)
      let block = Block b
      spaces
      end <- terminators
      case end of
        "elseif" -> do
          right <- elseIfHelper
          return $ IfStat cond block right
        "else" -> do
          spaces
          right <- parseBlock
          return $ IfStat cond block right
        "end" -> do
          return $ IfStat cond block Empty

-- whileStat := 'while' expr 'do' block
parseWhileStat :: Parser SyntaxTree
parseWhileStat = do
  reserved "while"
  spaces
  cond <- parseExpr
  spaces
  reserved "do"
  block <- parseBlock
  return $ WhileStat cond block

-- stat := expr
parseStat :: Parser SyntaxTree
parseStat = do
  try parseWhileStat
  <|> try parseIfStat
  <|> try parseAssign
  <|> parseExpr

-- block := stat+ end
parseBlock :: Parser SyntaxTree
parseBlock = do
  stats <- manyTill parseStat (spaces >> reserved "end")
  return $ Block stats

-- chunk := stat+
parseChunk :: Parser SyntaxTree
parseChunk = do
  parseStat `sepBy` spaces >>= \s -> return $ Chunk s

readExpr :: String -> Either String SyntaxTree
readExpr input = case parse parseChunk "says" input of
  Left err -> Left ("error: " ++ show err)
  Right expr -> Right expr