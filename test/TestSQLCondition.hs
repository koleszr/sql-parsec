module TestSQLCondition (tests) where

import SQLCondition
import SQLParSecUtils (SQLValue (..))
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestUtils
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

tests = [ testGroup_parseSqlCondition
        , testGroup_show_SQLConditionType
        , testGroup_read_SQLConditionType
        ]

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

testGroup_show_SQLConditionType =
  testGroup
  "SQLConditionType show"
  [ testCase "Should show equals" test_show_E
  , testCase "Should show not equals" test_show_NE
  , testCase "Should show greater than" test_show_GRT
  , testCase "Should show greater that or equals" test_show_GTE
  , testCase "Should show less than" test_show_LET
  , testCase "Should show less than or equals" test_show_LTE
  ]

test_show_E :: Assertion
test_show_E = "=" @=? show E

test_show_NE :: Assertion
test_show_NE = "<>" @=? show NE

test_show_GRT :: Assertion
test_show_GRT = ">" @=? show GRT

test_show_GTE :: Assertion
test_show_GTE = ">=" @=? show GTE

test_show_LET :: Assertion
test_show_LET = "<" @=? show LET

test_show_LTE :: Assertion
test_show_LTE = "<=" @=? show LTE

testGroup_read_SQLConditionType =
  testGroup
  "SQLConditionType read"
  [ testCase "Should read equals" test_read_E
  , testCase "Should read not equals" test_read_NE
  , testCase "Should read greater than" test_read_GRT
  , testCase "Should read greater that or equals" test_read_GTE
  , testCase "Should read less than" test_read_LET
  , testCase "Should read less than or equals" test_read_LTE
  , testCase "Should fail on invalid input" test_read_invalid
  ]

test_read_E :: Assertion
test_read_E = E @=? read "="

test_read_NE :: Assertion
test_read_NE = NE @=? read "<>"

test_read_GRT :: Assertion
test_read_GRT = GRT @=? read ">"

test_read_GTE :: Assertion
test_read_GTE = GTE @=? read ">="

test_read_LET :: Assertion
test_read_LET = LET @=? read "<"

test_read_LTE :: Assertion
test_read_LTE = LTE @=? read "<="

test_read_invalid :: Assertion
test_read_invalid = (Nothing :: Maybe SQLConditionType) @=? (readMaybe "INV")
