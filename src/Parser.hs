module Parser where

import Text.Megaparsec (Parsec, empty, oneOf, between, many, sepBy1, (<|>), MonadParsec (notFollowedBy, try), sepBy, runParser, errorBundlePretty)
import Data.Void (Void)
import Data.Text (Text)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1)
import Types (Term(..), Expr(..), Header(..))
import Data.Foldable (asum)
import qualified Data.Text as T
import Control.Monad (void)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1
  empty
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

multyP :: Parser Term
multyP = lexeme do
  res <- asum
    [ try $ between (symbol "(") (symbol ")") termP
    , TVar . T.singleton <$> oneOf ['a'..'z']
    , TZero <$ symbol "0"
    ]

  cnt <- length <$> many (symbol "'")
  pure $ addNext res cnt
  where
    addNext :: Term -> Int -> Term
    addNext term 0 = term
    addNext term n = TNext $ addNext term (n - 1)

addP :: Parser Term
addP = lexeme do
  terms <- multyP `sepBy1` symbol "*"
  pure $ foldl1 (:*:) terms

termP :: Parser Term
termP = lexeme do
  terms <- addP `sepBy1` symbol "+"
  pure $ foldl1 (:+:) terms

unaryP :: Parser Expr
unaryP = lexeme $ asum
  [ EPred . T.singleton <$> oneOf ['A'..'Z']

  , do
    void $ symbol "!"
    ENeg <$> unaryP

  , do
    quantor <- (EForall <$ symbol "@") <|> (EExists <$ symbol "?")
    var <- lexeme $ T.singleton <$> oneOf ['a'..'z']
    void $ symbol "."
    quantor var <$> exprP

  , do
    a <- termP
    void $ symbol "="
    b <- termP
    pure $ a :=: b

  , try $ between (symbol "(") (symbol ")") exprP
  ]

conjunctionP :: Parser Expr
conjunctionP = lexeme do
  exprs <- unaryP `sepBy1` symbol "&"
  pure $ foldl1 (:&:) exprs

disjunctionP :: Parser Expr
disjunctionP = lexeme do
  exprs <- conjunctionP `sepBy1` try (symbol "|" <* notFollowedBy "-")
  pure $ foldl1 (:|:) exprs

exprP :: Parser Expr
exprP = lexeme do
  exprs <- disjunctionP `sepBy1` symbol "->"
  pure $ foldr1 (:->:) exprs

headerP :: Parser Header
headerP = lexeme do
  hContext <- exprP `sepBy` symbol ","
  void $ symbol "|-"
  hToProve <- exprP
  pure Header{..}

parseHeader :: Text -> Header
parseHeader txt =
  case runParser headerP "" txt of
    Left err -> error $ errorBundlePretty err
    Right header -> header

parseExpression :: Text -> Expr
parseExpression txt =
  case runParser exprP "" txt of
    Left err -> error $ errorBundlePretty err
    Right expr -> expr
