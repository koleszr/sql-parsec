module SQLClause
  ( SQLClause(..)
  , SQLClauseType(..)
  , SQLOrdering(..)
  , SQLOrderingType(..)
  , parseSQLClauses
  , parseWhereClause
  ) where

import Control.Applicative (empty, (<|>))
import Data.Char (isSpace, toUpper)
import Data.List (intercalate, isPrefixOf, unfoldr)
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import SQLConditionCombinator
import SQLParSecUtils
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

data SQLClause
  = Where SQLConditionCombinator
  | GroupBy [String]
  | Having SQLConditionCombinator
  | OrderBy [SQLOrdering]
  deriving (Eq)

instance Show SQLClause where
  show (Where cc) = "WHERE " ++ show cc
  show (GroupBy groups) = "GROUP BY " ++ intercalate ", " groups
  show (Having cc) = "HAVING " ++ show cc
  show (OrderBy orderings) = "ORDER BY " ++ intercalate ", " (show <$> orderings)

parseSQLClauses :: ReadP [SQLClause]
parseSQLClauses = parseSQLClauseType >>= recur []
  where
    recur acc Nothing = return acc
    recur acc (Just typ) = do
      skipSpaces
      (c, nextType) <-
        (\(clause, str) -> (clause, readMaybe str)) <$> parseSQLClause typ
      recur (acc ++ [c]) nextType

parseSQLClause :: SQLClauseType -> ReadP (SQLClause, String)
parseSQLClause WHERE =
  (\(c, tr) -> (Where c, tr)) <$>
  parseSQLConditionCombinator untilNextSQLClauseTypeOrSemicolon
parseSQLClause HAVING =
  (\(c, tr) -> (Having c, tr)) <$>
  parseSQLConditionCombinator untilNextSQLClauseTypeOrSemicolon
parseSQLClause GROUPBY =
  (\(c, tr) -> (GroupBy c, tr)) <$>
  parseCommaSeparatedFields untilNextSQLClauseTypeOrSemicolon
parseSQLClause ORDERBY = (\(c, tr) -> (OrderBy c, tr)) <$> parseSQLOrderings

parseWhereClause :: ReadP (Either String (Maybe SQLClause))
parseWhereClause = parseSQLClauses >>= return . zeroOrOne . (filter isWhere)
  where
    isWhere (Where _) = True
    isWhere _ = False

data SQLClauseType
  = WHERE
  | HAVING
  | GROUPBY
  | ORDERBY

instance Read SQLClauseType where
  readsPrec _ str =
    case fmap toUpper str of
      "WHERE" -> [(WHERE, "")]
      "HAVING" -> [(HAVING, "")]
      "GROUP BY" -> [(GROUPBY, "")]
      "ORDER BY" -> [(ORDERBY, "")]
      _ -> []

sqlClauseTypes = ["where", "having", "order by", "group by"]

parseSQLClauseTypeString :: ReadP String
parseSQLClauseTypeString =
  foldl
    (\p typ -> p <++ string typ)
    empty
    (sqlClauseTypes ++ ((toUpper <$>) <$> sqlClauseTypes))

parseSQLClauseType :: ReadP (Maybe SQLClauseType)
parseSQLClauseType = parseSQLClauseTypeString >>= return . readMaybe

untilNextSQLClauseType = skipSpaces >> parseSQLClauseTypeString
untilNextSQLClauseTypeOrSemicolon = untilNextSQLClauseType <++ string ";"

data SQLOrderingType
  = ASC
  | DESC
  deriving (Eq, Show)

instance Read SQLOrderingType where
  readsPrec _ str =
    case toUpper <$> str of
      "ASC" -> [(ASC, "")]
      "DESC" -> [(DESC, "")]
      _ -> []

sqlOrderingTypes = ["asc", "desc"]

parseSQLOrderingTypeString :: ReadP String
parseSQLOrderingTypeString =
  foldl
    (\p typ -> p <++ string typ)
    empty
    (sqlOrderingTypes ++ ((toUpper <$>) <$> sqlOrderingTypes))      

untilNextSQLOrderingType = skipSpaces >> parseSQLOrderingTypeString

sepOrEndSQLOrdering = untilNextSQLOrderingType <++ untilNextSQLClauseTypeOrSemicolon

data SQLOrdering = SQLOrdering [String] SQLOrderingType deriving (Eq)

instance Show SQLOrdering where
  show (SQLOrdering columns ordering) =
    intercalate ", " columns ++ " " ++ show ordering

parseSQLOrderings :: ReadP ([SQLOrdering], String)
parseSQLOrderings = go []
  where
    go acc = do
      optional $ char ',' >> skipSpaces
      (cs, t) <- parseCommaSeparatedFields sepOrEndSQLOrdering
      skipSpaces
      case (cs, readMaybe t) of
        ([], Nothing) -> return $ (acc, t)
        (_, Nothing) -> return $ (acc ++ [SQLOrdering cs ASC], t)
        (_, Just typ) -> go $ acc ++ [SQLOrdering cs typ]
