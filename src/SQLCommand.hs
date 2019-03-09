module SQLCommand
  ( SQLCommand(..)
  , SQLCommandType(..)
  , parseSQLCommand
  ) where

import Control.Applicative ((<|>))
import Data.Char (isSpace, toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import SQLClause
import SQLColumn
import SQLParSecUtils
import Text.ParserCombinators.ReadP

type Table = String
type Columns = [SQLColumn]
type Clauses = [(SQLClauseType, [String])]

data SQLCommand = Select Table Columns Clauses
                | Insert Table Columns
                | Delete Table Clauses
                | Update Table Columns Clauses
                deriving (Eq)

instance Show SQLCommand where
  showsPrec _ (Select table columns clauses) =
    showString "SELECT " .
    (showString $ intercalate ", " (showColumnName <$> columns)) .
    showString " FROM " .
    showString table .
    if null clauses
      then showChar ';'
      else (showChar ' ' .
            (showString $ intercalate " " (showClause <$> clauses)) .
            showChar ';')
  showsPrec _ (Insert table columnsWithValue) =
    showString "INSERT INTO " .
    showString table .
    showString " (" .
    (showString $ intercalate ", " (showColumnName <$> columnsWithValue)) .
    showString ") VALUES (" .
    (showString $
     intercalate ", " ((fromMaybe "") . showColumnValue <$> columnsWithValue)) .
    showString ");"
  showsPrec _ (Delete table clauses) =
    showString "DELETE FROM " .
    showString table .
    showChar ' ' .
    (showString $ intercalate " " (showClause <$> clauses)) . showChar ';'
  showsPrec _ (Update table columns clauses) =
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

parseSQLCommand :: ReadP SQLCommand
parseSQLCommand = fmap (read . fst) (parseWord endWithSpace) >>= parseByType

parseByType :: SQLCommandType -> ReadP SQLCommand
parseByType SELECT = do
  (columns, _) <- parseCommaSeparatedFields $ satisfy isSpace
  string "from " <|> string "FROM "
  (table, last) <- parseWord endWithSpaceOrSemicolon
  clauses <- parseClauses last
  return $ Select table (column <$> columns) clauses
parseByType UPDATE = do
  (table, _) <- parseWord endWithSpace
  string "set " <|> string "SET "
  (columns, last) <- parseCommaSeparatedFields endWithSpaceOrSemicolon
  clauses <- parseClauses last
  return $ Update table (column <$> columns) clauses
parseByType DELETE = do
  string "from " <|> string "FROM "
  (table, last) <- parseWord endWithSpaceOrSemicolon
  clauses <- parseClauses last
  return $ Delete table clauses
parseByType INSERT = do
  string "into " <|> string "INTO "
  (table, _) <- parseWord $ string " ("
  (cs, _) <-
    parseCommaSeparatedFields (string ") values (" <|> string ") VALUES (")
  (vs, _) <- parseCommaSeparatedFields $ string ");"
  return $ Insert table (zipWith columnWithValue cs vs)
