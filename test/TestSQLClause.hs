module TestSQLClause (tests) where

import SQLClause
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestUtils
import Text.ParserCombinators.ReadP

tests = [testGroup_parseClause, testGroup_parseClauses]

testGroup_parseClause =
  testGroup
    "parseClause"
    [ testCase "Should parse WHERE clause correctly" test_parseClause_single
    , testCase
        "Should parse WHERE clause with multiple conditions correctly"
        test_parseClause_multiple
    ]

testGroup_parseClauses =
  testGroup
    "parseClauses"
    [ testCase
        "Should parse a single SQL clause correctly"
        test_parseClauses_single
    , testCase
        "Should parse multiple SQL clauses correcty"
        test_parseClauses_multiple
    , testCase
        "Should return an empty list if the input argument is not a space"
        test_parseClauses_noSpace
    ]

test_parseClause_single :: Assertion
test_parseClause_single =
  assertParse
    (Just (WHERE, ["age>27"], ';'))
    (parseClause $ satisfy (== ';'))
    "WHERE age>27;"

test_parseClause_multiple :: Assertion
test_parseClause_multiple =
  assertParse
    (Just (WHERE, ["age>27", "name='Zoltan'"], ';'))
    (parseClause $ satisfy (== ';'))
    "WHERE age>27, name='Zoltan';"

test_parseClauses_single :: Assertion
test_parseClauses_single =
  assertParse [(WHERE, ["age>27"])] (parseClauses ' ') "WHERE age>27;"

test_parseClauses_multiple :: Assertion
test_parseClauses_multiple =
  assertParse
    [ (WHERE, ["age>27", "name='Zoltan'"])
    , (HAVING, ["COUNT(*)"])
    , (ORDERBY, ["name", "age"])
    ]
    (parseClauses ' ')
    "WHERE age>27, name='Zoltan' HAVING COUNT(*) ORDER BY name, age;"

test_parseClauses_noSpace :: Assertion
test_parseClauses_noSpace = assertParse [] (parseClauses ';') "WHERE age>27;"
