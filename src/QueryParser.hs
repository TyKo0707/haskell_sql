module QueryParser (SQLQuery(..), parseSQLQuery) where

import Control.Applicative (empty)
import Data.Functor (void)
import Data.Void
import Data.Char (isAlphaNum)
import SQLKeywords

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data SQLQuery = SQLQuery
  { selectCols :: [String]
  , fromSource :: String
  , whereClause :: Maybe String
  , orderBy     :: Maybe String
  , limitCount  :: Maybe Int
  } deriving (Show, Eq)

-- Whitespace + comment skipper
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

sqlQueryParser :: Parser SQLQuery
sqlQueryParser = do
  _ <- lexeme (string' selectKW)
  selectedCols <- identifier `sepBy1` symbol ","
  _ <- lexeme (string' fromKW)
  tableName <- identifier
  maybeWhere <- optional (try $ lexeme (string' whereKW) >> manyTill anySingle (lookAhead $ try (void (string' orderByKW) <|> void (string' limitKW) <|> eof)))
  maybeOrder <- optional (try $ lexeme (string' orderByKW) >> identifier)
  maybeLimit <- optional (try $ lexeme (string' limitKW) >> L.decimal)
  eof
  return $ SQLQuery selectedCols tableName (cleanup maybeWhere) maybeOrder maybeLimit
  where
    cleanup = fmap (unwords . words)  -- just strip redundant spaces

parseSQLQuery :: String -> Either String SQLQuery
parseSQLQuery input =
  case parse sqlQueryParser "" input of
    Left err -> Left (errorBundlePretty err)
    Right query -> Right query
