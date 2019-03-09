module SQLParSec where

import Data.Char (isSpace, toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, maybeToList, catMaybes)
import Control.Applicative (empty, (<|>))
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

data SQLCommand = SQLCommand
  { sqlCommandType :: SQLCommandType
  , table :: String
  , columns :: [SQLColumn]
  , clauses :: [(SQLClauseType, [String])]
  } deriving (Eq)

instance Show SQLCommand where
  showsPrec _ (SQLCommand SELECT table columns clauses) =
    showString "SELECT " .
    (showString $ intercalate ", " (showColumnName <$> columns)) .
    showString " FROM " .
    showString table .
    if null clauses
      then showChar ';'
      else (showChar ' ' .
            (showString $ intercalate " " (showClause <$> clauses)) .
            showChar ';')
  showsPrec _ (SQLCommand INSERT table columnsWithValue _) =
    showString "INSERT INTO " .
    showString table .
    showString " (" .
    (showString $ intercalate ", " (showColumnName <$> columnsWithValue)) .
    showString ") VALUES (" .
    (showString $
     intercalate ", " ((fromMaybe "") . showColumnValue <$> columnsWithValue)) .
    showString ");"
  showsPrec _ (SQLCommand DELETE table [] clauses) =
    showString "DELETE FROM " .
    showString table .
    showChar ' ' .
    (showString $ intercalate " " (showClause <$> clauses)) . showChar ';'
  showsPrec _ (SQLCommand UPDATE table columns clauses) =
    showString "UPDATE " .
    showString table .
    showString " SET " .
    (showString $ intercalate ", " (showColumnName <$> columns)) .
    if null clauses
      then showChar ';'
      else (showChar ' ' .
            (showString $ intercalate " " (showClause <$> clauses)) .
            showChar ';')

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

data SQLColumn = Column String
               | ColumnWithString String String
               deriving (Eq, Show)

column :: String -> SQLColumn
column = Column

columnWithValue :: String -> String -> SQLColumn
columnWithValue = ColumnWithString

showColumnName :: SQLColumn -> String
showColumnName (Column c) = c
showColumnName (ColumnWithString c _) = c

showColumnValue :: SQLColumn -> Maybe String
showColumnValue (ColumnWithString _ v) = Just v
showColumnValue _ = Nothing

data SQLClauseType = WHERE
                   | HAVING
                   | GROUPBY
                   | ORDERBY
                   deriving (Eq)

instance Read SQLClauseType where
  readsPrec _ str =
    case fmap toUpper str of
      "WHERE" -> [(WHERE, "")]
      "HAVING" -> [(HAVING, "")]
      "GROUP BY" -> [(GROUPBY, "")]
      "ORDER BY" -> [(ORDERBY, "")]
      _ -> []

instance Show SQLClauseType where
  showsPrec _ WHERE = showString "WHERE"
  showsPrec _ HAVING = showString "HAVING"
  showsPrec _ GROUPBY = showString "GROUP BY"
  showsPrec _ ORDERBY = showString "ORDER BY"

parseSQLClauseType :: ReadP (Maybe SQLClauseType)
parseSQLClauseType =
  foldl
    (\p t -> p <|> string t)
    empty
    (sqlClauseTypes ++ ((toUpper <$>) <$> sqlClauseTypes)) >>=
  return . readMaybe

sqlClauseTypes = ["where", "having", "order by", "group by"]

parseSQLCommand :: ReadP SQLCommand
parseSQLCommand = fmap (read . fst) (parseWord endWithSpace) >>= parseByType

parseByType :: SQLCommandType -> ReadP SQLCommand
parseByType SELECT = do
  (columns, _) <- parseCommaSeparatedFields $ satisfy isSpace
  string "from " <|> string "FROM "
  (table, last) <- parseWord endWithSpaceOrSemicolon
  clauses <- parseClauses last
  return $ SQLCommand SELECT table (column <$> columns) clauses
parseByType UPDATE = do
  (table, _) <- parseWord endWithSpace
  string "set " <|> string "SET "
  (columns, last) <- parseCommaSeparatedFields endWithSpaceOrSemicolon
  clauses <- parseClauses last
  return $ SQLCommand UPDATE table (column <$> columns) clauses
parseByType DELETE = do
  string "from " <|> string "FROM "
  (table, last) <- parseWord endWithSpaceOrSemicolon
  clauses <- parseClauses last
  return $ SQLCommand DELETE table [] clauses
parseByType INSERT = do
  string "into " <|> string "INTO "
  (table, _) <- parseWord $ string " ("
  (cs, _) <-
    parseCommaSeparatedFields (string ") values (" <|> string ") VALUES (")
  (vs, _) <- parseCommaSeparatedFields $ string ");"
  return $ SQLCommand INSERT table (zipWith columnWithValue cs vs) []

parseClause :: ReadP a -> ReadP (Maybe (SQLClauseType, [String], a))
parseClause trail =
  parseSQLClauseType >>=
  (\m -> satisfy (== ' ') >> (sequence $ (p trail) <$> m))
  where
    p trail =
      (\t -> (\(fs, v) -> (t, fs, v)) <$> (parseCommaSeparatedFields trail))

parseClauses :: Char -> ReadP [(SQLClauseType, [String])]
parseClauses ' ' = do
  initClauses <- catMaybes <$> (many (parseClause endWithSpace))
  last <-
    (\m -> (\(ct, cs, _) -> (ct, cs)) <$> maybeToList m) <$>
    (parseClause endWithSemicolon)
  return $ (fmap (\(t, cs, _) -> (t, cs)) initClauses) ++ last
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

showClause :: (SQLClauseType, [String]) -> String
showClause (typ, vs) = show typ ++ " " ++ intercalate ", " vs
