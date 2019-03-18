module SQLCondition where

import Control.Applicative (empty)
import Data.Char (toUpper)
import Data.List (intercalate)
import SQLParSecUtils
import Text.ParserCombinators.ReadP

data SQLCondition =
  SQLCondition SQLConditionType
               String
               SQLValue
  deriving (Eq)

instance Show SQLCondition where
  show (SQLCondition typ name value) =
    name ++ " " ++ show typ ++ " " ++ show value

parseSQLCondition :: ReadP a -> ReadP (SQLCondition, a)
parseSQLCondition trail = do
  name <- fst <$> (parseWord skipSpaces)
  typ <- fst <$> (parseSQLConditionType skipSpaces)
  (value, t) <- (parseString trail <++ parseNumber trail)
  return (SQLCondition typ name value, t)

data SQLConditionType
  = E
  | NE
  | GRT
  | GTE
  | LET
  | LTE
  deriving (Eq)

instance Show SQLConditionType where
  show E = "="
  show NE = "<>"
  show GRT = ">"
  show GTE = ">="
  show LET = "<"
  show LTE = "<="

instance Read SQLConditionType where
  readsPrec _ "=" = [(E, "")]
  readsPrec _ "<>" = [(NE, "")]
  readsPrec _ ">" = [(GRT, "")]
  readsPrec _ ">=" = [(GTE, "")]
  readsPrec _ "<" = [(LET, "")]
  readsPrec _ "<=" = [(LTE, "")]
  readsPrec _ _ = []                    

sqlConditionsType = ["=", "<>", ">=", ">", "<=", "<"]

parseSQLConditionType :: ReadP a -> ReadP (SQLConditionType, a)
parseSQLConditionType trail = do
  typ <- read <$> foldl (\p t -> p <++ string t) empty sqlConditionsType
  t <- trail
  return (typ, t)
