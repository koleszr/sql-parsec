module SQLParSecUtils where

import Data.Char (isSpace)
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

parseWord :: ReadP a -> ReadP (String, a)
parseWord trail = do
  word <- many1 $ satisfy (\c -> not (isSpace c) && (not (elem c ".,:;")))
  trailingChar <- trail
  return (word, trailingChar)

parseCommaSeparatedFields :: ReadP a -> ReadP ([String], a)
parseCommaSeparatedFields trail = do
  initWords <- option [] (many1 $ parseWord (string ", "))
  (lastWord, last) <- parseWord trail
  return $ ((fmap fst initWords) ++ [lastWord], last)

endWithSpace :: ReadP Char
endWithSpace = satisfy isSpace

endWithSemicolon :: ReadP Char
endWithSemicolon = satisfy (== ';')

endWithSpaceOrSemicolon :: ReadP Char
endWithSpaceOrSemicolon = endWithSpace <|> endWithSemicolon
