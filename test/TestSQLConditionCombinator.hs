module TestSQLConditionCombinator (tests) where

import SQLCondition
import SQLConditionCombinator
import SQLParSecUtils (SQLValue (..))
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestUtils
import Text.ParserCombinators.ReadP

tests = [ testCase_pure
        , testCase_parseAnd
        , testCase_parseOr
        , testCase_parseSQLConditionCombinator
        ]

testCase_pure = testCase "Should parse pure condition" test_parsePure

testCase_parseAnd = testCase "Should parse AND condition" test_parseAnd

testCase_parseOr = testCase "Should parse OR condition" test_parseOr

testCase_parseSQLConditionCombinator =
  testCase "Should parse conditions" test_parseSQLConditionCombinator

test_parsePure :: Assertion
test_parsePure =
  assertParse
    (Pure $ SQLCondition E "name" (TextValue "Zoltan Koleszar"), ';')
    (parseSQLConditionCombinator $ char ';')
    "name = 'Zoltan Koleszar';"

test_parseAnd :: Assertion
test_parseAnd =
  assertParse
    ((Bin
        AND
        (Pure $ SQLCondition E "name" (TextValue "Zoltan Koleszar"))
        (Pure $ SQLCondition GRT "age" (NumberValue "27"))), ';')
    (parseBin $ char ';')
    "name = 'Zoltan Koleszar' AND age > 27;"

test_parseOr :: Assertion
test_parseOr =
  assertParse
    ((Bin
        OR
        (Pure $ SQLCondition E "name" (TextValue "Zoltan Koleszar"))
        (Pure $ SQLCondition GTE "age" (NumberValue "27"))), ';')
    (parseBin $ char ';')
    "name = 'Zoltan Koleszar' OR age >= 27;"

test_parseSQLConditionCombinator :: Assertion
test_parseSQLConditionCombinator =
  assertParse
    ((Bin
        AND
        (Pure $ SQLCondition E "name" (TextValue "Zoltan"))
        (Bin
           OR
           (Pure $ SQLCondition GTE "age" (NumberValue "27"))
           (Bin
              AND
              (Pure $ SQLCondition E "employer" (TextValue "E Corp"))
              (Bin
                 OR
                 (Pure $ SQLCondition E "degree" (TextValue "MSc"))
                 (Pure $ SQLCondition LET "salary" (NumberValue "100000")))))), ';')
    (parseSQLConditionCombinator $ char ';')
    "name = 'Zoltan' AND age >= 27 OR employer = 'E Corp' AND degree = 'MSc' OR salary < 100000;"
