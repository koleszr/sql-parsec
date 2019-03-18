module SQLParSecUtils where

import Data.Char (isNumber, isSpace)
import Data.List (dropWhileEnd)
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

data SQLValue = TextValue String
              | NumberValue String
              deriving Eq

instance Show SQLValue where
  show (TextValue str) = "'" ++ str ++ "'"
  show (NumberValue n) = n 

word :: ReadP String
word = many1 $ satisfy (\c -> (not $ isSpace c) && (not $ elem c ",:;"))

parseWord :: ReadP a -> ReadP (String, a)
parseWord trail = do
  w <- word
  trailingChar <- trail
  return (w, trailingChar)

parseCommaSeparatedFields :: ReadP a -> ReadP ([String], a)
parseCommaSeparatedFields trail = do
  fields <- sepBy word (string ", ")
  t <- trail
  return (fields, t)

parseString :: ReadP a -> ReadP (SQLValue, a)
parseString trail = do
  str <- TextValue <$> (between (char '\'') (char '\'') (munch1 $ (/= '\'')) <++
   (between (char '"') (char '"') (munch1 $ (/= '"'))))
  t <- trail
  return (str, t)

parseNumber :: ReadP a -> ReadP (SQLValue, a)
parseNumber trail = do
  i <- munch1 isNumber
  dot <- option "" (string ".")
  f <- option "" (munch1 isNumber)
  t <- trail
  return (NumberValue $ i ++ dot ++ f, t)

endWithSpace :: ReadP Char
endWithSpace = satisfy isSpace

endWithSemicolon :: ReadP Char
endWithSemicolon =  char ';'

endWithComma :: ReadP Char
endWithComma = char ','

endWithSpaceOrSemicolon :: ReadP Char
endWithSpaceOrSemicolon = endWithSpace <++ endWithSemicolon

endWithCommaOrSemicolon :: ReadP Char
endWithCommaOrSemicolon = endWithComma <++ endWithSemicolon

endWithCommaOrSpace :: ReadP Char
endWithCommaOrSpace = endWithComma <++ endWithSpace

split :: Eq a => a -> [a] -> [[a]]
split delim values = s delim values []
  where
    s d [] acc = [acc]
    s d (c:cs) acc
      | d == c = [acc] ++ (s d cs [])
      | otherwise = s d cs (acc ++ [c])

trim :: String -> String
trim = (dropWhile isSpace) . (dropWhileEnd isSpace)

zeroOrOne :: [a] -> Either String (Maybe a)
zeroOrOne [] = Right Nothing
zeroOrOne (elem:[]) = Right (Just elem)
zeroOrOne _ = Left "Found more than one element!"
