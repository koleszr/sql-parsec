module SQLClause
  ( SQLClauseType (..)
  , showClause
  , parseClause
  , parseClauses
  ) where

import Control.Applicative (empty, (<|>))
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Maybe (catMaybes, maybeToList)
import SQLParSecUtils
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

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

showClause :: (SQLClauseType, [String]) -> String
showClause (typ, vs) = show typ ++ " " ++ intercalate ", " vs

parseSQLClauseType :: ReadP (Maybe SQLClauseType)
parseSQLClauseType =
  foldl
    (\p t -> p <|> string t)
    empty
    (sqlClauseTypes ++ ((toUpper <$>) <$> sqlClauseTypes)) >>=
  return . readMaybe

sqlClauseTypes = ["where", "having", "order by", "group by"]


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