module TestSQLCondition (tests) where

import SQLCondition
import SQLParSecUtils (SQLValue (..))
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestUtils
import Text.ParserCombinators.ReadP

tests = [ testGroup_parseSqlCondition ]

testGroup_parseSqlCondition =
  testGroup
  "SQL conditions"
  [ testCase "Should parse equals condition" test_parseSQLCondition_e
  , testCase "Should parse not equals condition" test_parseSQLCondition_ne
  , testCase "Should parse greater than condition" test_parseSQLCondition_grt
  , testCase "Should parse greater than or equal to condition" test_parseSQLCondition_gte
  , testCase "Should parse less than condition" test_parseSQLCondition_let
  , testCase "Should parse less than or equal to condition" test_parseSQLCondition_lte
  ]

test_parseSQLCondition_e :: Assertion
test_parseSQLCondition_e =
  assertParse
    (SQLCondition E "name" (TextValue "Zoltan"), ';')
    (parseSQLCondition $ char ';')
    "name = 'Zoltan';"

test_parseSQLCondition_ne :: Assertion
test_parseSQLCondition_ne =
  assertParse
    (SQLCondition NE "name" (TextValue "Zoltan Koleszar"), ';')
    (parseSQLCondition $ char ';')
    "name <> 'Zoltan Koleszar';"

test_parseSQLCondition_grt :: Assertion
test_parseSQLCondition_grt =
  assertParse
    (SQLCondition GRT "age" (NumberValue "27"), ';')
    (parseSQLCondition $ char ';')
    "age > 27;"

test_parseSQLCondition_gte :: Assertion
test_parseSQLCondition_gte =
  assertParse
    (SQLCondition GTE "age" (NumberValue "27"), ';')
    (parseSQLCondition $ char ';')
    "age >= 27;"

test_parseSQLCondition_let :: Assertion
test_parseSQLCondition_let =
  assertParse
    (SQLCondition LET "age" (NumberValue "27"), ';')
    (parseSQLCondition $ char ';')
    "age < 27;"

test_parseSQLCondition_lte :: Assertion
test_parseSQLCondition_lte =
  assertParse
    (SQLCondition LTE "age" (NumberValue "27"), ';')
    (parseSQLCondition $ char ';')
    "age <= 27;"
