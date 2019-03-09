module TestUtils where

import Text.ParserCombinators.ReadP
import Test.HUnit

assertParse :: (Eq a, Show a) => a -> ReadP a -> String -> Assertion
assertParse expected parser str =
  case readP_to_S parser str of
    [(actual, _)] -> expected @=? actual
    otherwise -> assertFailure $ "Failure: " ++ show otherwise
