module SQLConditionCombinator where

import Control.Applicative (empty, (<|>))
import Data.Char (toUpper)
import Data.List (intercalate)
import SQLCondition
import Text.ParserCombinators.ReadP

data SQLConditionCombinator
  = Pure SQLCondition
  | Not SQLCondition
  | Bin SQLConditionCombinatorType SQLConditionCombinator SQLConditionCombinator
  | Group [SQLConditionCombinator]
  deriving (Eq)

instance Show SQLConditionCombinator where
  show (Pure c) = show c
  show (Not c) = "NOT " ++ show c
  show (Bin op l r) = show l ++ " " ++ show op ++ " " ++ show r
  show (Group cs) = "(" ++ (intercalate " " (show <$> cs)) ++ ")"

instance Read SQLConditionCombinator where
  readsPrec _ str = readP_to_S (fst <$> (parseSQLConditionCombinator eof)) str

data SQLConditionCombinatorType = NOT
                                | AND
                                | OR
                                deriving (Eq, Show)

instance Read SQLConditionCombinatorType where
  readsPrec _ str =
    case toUpper <$> str of
      "NOT" -> [(NOT, "")]
      "AND" -> [(AND, "")]
      "OR" -> [(OR, "")]
      _ -> []

andReadP = foldl (\p o -> p <++ string o) empty ["AND", "and"]
orReadP = foldl (\p o -> p <++ string o) empty ["OR", "or"]
binReadP = andReadP <++ orReadP  
  
unarySQLConditionCombinators = ["NOT", "not"]

parseSQLConditionCombinator :: ReadP a -> ReadP (SQLConditionCombinator, a)
parseSQLConditionCombinator trail = (parsePure trail) <++ (parseBin trail)

parsePure :: ReadP a -> ReadP (SQLConditionCombinator, a)
parsePure trail = (\(c, t) -> (Pure c, t)) <$> parseSQLCondition trail

parseBin :: ReadP a -> ReadP (SQLConditionCombinator, a)
parseBin trail = do
  left <- fst <$> parseSQLConditionCombinator skipSpaces
  op <- read <$> binReadP
  skipSpaces
  (right, t) <- parseSQLConditionCombinator trail
  return (Bin op left right, t)
