module SQLParSec where

import Data.Char (isSpace, toUpper)
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

data SQLCommand = SQLCommand
  { sqlCommandType :: SQLCommandType
  , table :: String
  , columns :: [String]
  , clauses :: [(SQLClauseType, [String])]
  } deriving (Eq, Show)

data SQLCommandType = SELECT
                    | INSERT
                    | UPDATE
                    | DELETE
                    deriving (Eq, Show)

instance Read SQLCommandType where
  readsPrec _ str =
    case fmap toUpper str of
      "SELECT" -> [(SELECT, "")]
      "INSERT" -> [(INSERT, "")]
      "UPDATE" -> [(UPDATE, "")]
      "DELETE" -> [(DELETE, "")]
      _ -> []

data SQLClauseType = WHERE
                   | HAVING
                   | GROUPBY
                   | ORDERBY
                   deriving (Eq, Show)

instance Read SQLClauseType where
  readsPrec _ str =
    case fmap toUpper str of
      "WHERE" -> [(WHERE, "")]
      "HAVING" -> [(HAVING, "")]
      "GROUPBY" -> [(GROUPBY, "")]
      "ORDERBY" -> [(ORDERBY, "")]
      _ -> []

parseSQLCommand :: ReadP SQLCommand
parseSQLCommand = fmap (read . fst) (parseWord $ endWithSpace) >>= parseByType

parseByType :: SQLCommandType -> ReadP SQLCommand
parseByType SELECT = do
  (columns, _) <- parseCommaSeparatedFields $ satisfy isSpace
  string "from " <|> string "FROM "
  (table, last) <- parseWord $ endWithSpaceOrSemicolon
  clauses <- parseClauses last
  return $ SQLCommand SELECT table columns clauses
parseByType UPDATE = do
  (table, _) <- parseWord $ endWithSpace
  string "set " <|> string "SET "
  (columns, last) <- parseCommaSeparatedFields $ endWithSpaceOrSemicolon
  clauses <- parseClauses last
  return $ SQLCommand UPDATE table columns clauses
parseByType DELETE = do
  string "from " <|> string "FROM "
  (table, last) <- parseWord $ endWithSpaceOrSemicolon
  clauses <- parseClauses last
  return $ SQLCommand DELETE table [] clauses
parseByType INSERT = do
  string "into " <|> string "INTO "
  (table, _) <- parseWord $ string " ("
  (cs, _) <- parseCommaSeparatedFields $ string ") "
  string "values (" <|> string "VALUES ("
  (vs, _) <- parseCommaSeparatedFields $ string ");"
  return $ SQLCommand INSERT table (zipWith (\a b -> a ++ "=" ++ b) cs vs) []

parseClause :: ReadP a -> ReadP (SQLClauseType, [String], a)
parseClause trail = do
  clauseType <- fmap (read . fst)  (parseWord $ endWithSpace)
  (fields, trailing) <- parseCommaSeparatedFields $ trail
  return (clauseType, fields, trailing)

parseClauses :: Char -> ReadP [(SQLClauseType, [String])]
parseClauses ' ' = do
  initClauses <- option [] (many1 (parseClause $ endWithSpace))
  (lastClauseType, lastConditions, _) <- parseClause $ endWithSemicolon
  return $
    (fmap (\(t, cs, _) -> (t, cs)) initClauses) ++
    [(lastClauseType, lastConditions)]
parseClauses _ = return []

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
