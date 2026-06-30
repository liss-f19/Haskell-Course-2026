module MathLang.Parser
  ( parseProgram
  ) where

import Control.Monad (void)
import Data.Functor (($>))
import Data.Void (Void)
import MathLang.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

reserved :: [String]
reserved = ["def", "let", "in"]

ident :: Parser String
ident = lexeme . try $ do
  first <- letterChar
  rest <- many (alphaNumChar <|> char '_')
  let name = first : rest
  if name `elem` reserved
    then fail ("reserved word " <> name <> " cannot be used as identifier")
    else pure name

numberP :: Parser Expr
numberP = ScalarLit <$> lexeme L.float

exprP :: Parser Expr
exprP = try letExprP <|> additiveP

letExprP :: Parser Expr
letExprP = do
  _ <- symbol "let"
  name <- ident
  _ <- symbol "="
  value <- exprP
  _ <- symbol "in"
  body <- exprP
  pure (Let name value body)

additiveP :: Parser Expr
additiveP = do
  lhs <- multiplicativeP
  rest lhs
  where
    rest lhs =
      (do
         op <- (symbol "+" $> Add) <|> (symbol "-" $> Sub)
         rhs <- multiplicativeP
         rest (BinOp op lhs rhs)
      ) <|> pure lhs

multiplicativeP :: Parser Expr
multiplicativeP = do
  lhs <- unaryP
  rest lhs
  where
    rest lhs =
      (do
         op <- (symbol "*" $> Mul) <|> (symbol "/" $> Div)
         rhs <- unaryP
         rest (BinOp op lhs rhs)
      ) <|> pure lhs

unaryP :: Parser Expr
unaryP = (symbol "-" *> (Neg <$> unaryP)) <|> atomP

atomP :: Parser Expr
atomP =
  choice
    [ try matrixP
    , vectorP
    , try callP
    , Var <$> ident
    , numberP
    , parens exprP
    ]

callP :: Parser Expr
callP = do
  fn <- ident
  args <- parens (exprP `sepBy` symbol ",")
  pure (Apply fn args)

vectorP :: Parser Expr
vectorP = VecLit <$> brackets (exprP `sepBy` symbol ",")

matrixP :: Parser Expr
matrixP = MatLit <$> brackets (row `sepBy` symbol ",")
  where
    row = brackets (exprP `sepBy` symbol ",")

definitionP :: Parser Definition
definitionP = try funcDefP <|> try varDefP

funcDefP :: Parser Definition
funcDefP = do
  _ <- symbol "def"
  name <- ident
  args <- parens (ident `sepBy` symbol ",")
  _ <- symbol "="
  body <- exprP
  pure (FuncDef name args body)

varDefP :: Parser Definition
varDefP = do
  name <- ident
  _ <- symbol "="
  value <- exprP
  pure (VarDef name value)

programP :: Parser Program
programP = do
  sc
  defs <- many (try (definitionP <* optional (symbol ";") <* sc))
  expr <- exprP
  optional (symbol ";")
  eof
  pure (Program defs expr)

parseProgram :: String -> Either String Program
parseProgram input =
  case runParser programP "MathLang" input of
    Left e -> Left (errorBundlePretty e)
    Right p -> Right p
