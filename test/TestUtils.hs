module TestUtils where

import Data.Char (toLower)
import Test.HUnit
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

assertParse :: (Eq a, Show a) => a -> ReadP a -> String -> Assertion
assertParse expected parser str =
  case readP_to_S parser str of
    [(actual, _)] -> expected @=? actual
    otherwise -> assertFailure $ "Failure: " ++ show otherwise

assertRead :: (Eq a, Read a, Show a) => Maybe a -> String -> Assertion
assertRead expected str =
  (expected @=? (readMaybe str)) >> (expected @=? (readMaybe $ toLower <$> str))

assertShow :: Show a => String -> a -> Assertion
assertShow expected a = expected @=? show a
