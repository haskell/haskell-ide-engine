module HaReGA1 where
import Text.ParserCombinators.Parsec

parseStr :: CharParser () String
parseStr = do
  char '"'
  str <- many1 (noneOf "\"")
  char '"'
  return str
