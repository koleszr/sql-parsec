module SQLCommand
  ( SQLCommand (..)
  , SQLCommandType
  , parseSQLCommand
  ) where

import Control.Applicative ((<|>))
import Data.Char (isSpace, toUpper)
import Data.Either (fromRight)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import SQLClause
import SQLParSecUtils
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

type Table = String
type Columns = [String]
type Values = [String]

data SQLCommand = Select Table Columns [SQLClause]
                | Insert Table Columns Values
                | Delete Table (Maybe SQLClause)
                | Update Table Columns (Maybe SQLClause)
                deriving (Eq)

instance Show SQLCommand where
  showsPrec _ (Select table columns clauses) =
    showString "SELECT " .
    (showString $ intercalate ", " columns) .
    showString " FROM " .
    showString table .
    if null clauses
      then showChar ';'
      else (showChar ' ' .
            (showString $ intercalate " " (show <$> clauses)) . showChar ';')
  showsPrec _ (Insert table columns values) =
    showString "INSERT INTO " .
    showString table .
    showString " (" .
    (showString $ intercalate ", " columns) .
    showString ") VALUES (" .
    (showString $ intercalate ", " values) . showString ");"
  showsPrec _ (Delete table clause) =
    showString "DELETE FROM " .
    showString table .
    (fromMaybe
       (showChar ';')
       ((\c -> showChar ' ' . (showString $ show c) . showChar ';') <$>
        clause))
  showsPrec _ (Update table columns clause) =
    showString "UPDATE " .
    showString table .
    showString " SET " .
    (showString $ intercalate ", " columns) .
    (fromMaybe
       (showChar ';')
       ((\c -> showChar ' ' . (showString $ show c) . showChar ';') <$>
        clause))

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

parseSQLCommand :: ReadP (Maybe SQLCommand)
parseSQLCommand = fmap (readMaybe . fst) (parseWord endWithSpace) >>= parseByType

parseByType :: Maybe SQLCommandType -> ReadP (Maybe SQLCommand)
parseByType t = sequence $ fmap p t
  where
    p SELECT = do
      (columns, _) <- parseCommaSeparatedFields $ satisfy isSpace
      string "FROM " <++ string "from "
      (table, last) <- parseWord endWithSpaceOrSemicolon
      clauses <- if last == ';' then return [] else parseSQLClauses
      return $ Select table columns clauses
    p UPDATE = do
      (table, _) <- parseWord endWithSpace
      string "SET " <++ string "set "
      (columns, last) <- parseCommaSeparatedFields endWithSpaceOrSemicolon
      clause <-
        if isSpace last
        then parseWhereClause
        else return . Right $  Nothing
      fromEither clause (\c -> return $ Update table columns c)
    p DELETE = do
      string "FROM " <++ string "from "
      (table, last) <- parseWord endWithSpaceOrSemicolon
      clause <-
        if isSpace last
        then parseWhereClause
        else return . Right $ Nothing
      fromEither clause (\c -> return $ Delete table c)
    p INSERT = do
      string "INTO " <++ string "into "
      (table, _) <- parseWord $ string " ("
      (cs, _) <-
        parseCommaSeparatedFields (string ") VALUES (" <++ string ") values (")
      (values, _) <- parseCommaSeparatedFields $ string ");"
      return $ Insert table cs values

fromEither :: Either e a -> (a -> ReadP SQLCommand) -> ReadP SQLCommand
fromEither e r = fromRight pfail (r <$> e)
