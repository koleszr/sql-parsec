module TestSQLClause (tests) where

import SQLClause
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestUtils
import Text.ParserCombinators.ReadP

tests =
  [ testGroup_parseClause
  , testGroup_parseClauses
  , testGroup_parseWhereClause
  , testGroup_read_SQLClauseType
  , testCase "Should show clause correctly" test_showClause
  ]

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

testGroup_parseWhereClause =
  testGroup
    "parseWhereClause"
    [ testCase
        "Should return WHERE clause if it was the only one provided"
        test_parseWhereClause
    , testCase
        "Should return Nothing if no clauses were provided"
        test_parseWhereClause_no_clause
    , testCase
        "Should fail if multiple clauses were provided"
        test_parseWhereClause_fail
    ]

testGroup_read_SQLClauseType =
  testGroup
    "read SQLClauseType"
    [ testCase "Should read \"WHERE\" or \"where\" as WHERE" test_read_WHERE
    , testCase "Should read \"HAVING\" or \"having\" as HAVING" test_read_HAVING
    , testCase
        "Should read \"GROUP BY\" or \"group by\" as GROUPBY"
        test_read_GROUPBY
    , testCase
        "Should read \"ORDER BY\" or \"order by\" as ORDERBY"
        test_read_ORDERBY
    , testCase "Should fail on invalid input" test_read_invalid
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

test_parseWhereClause :: Assertion
test_parseWhereClause =
  assertParse
    (Just (WHERE, ["age>27", "name='Zoltan'"]))
    (parseWhereClause ' ')
    "WHERE age>27, name='Zoltan';"

test_parseWhereClause_no_clause :: Assertion
test_parseWhereClause_no_clause =
  assertParse (Nothing) (parseWhereClause ';') ""

test_parseWhereClause_fail :: Assertion
test_parseWhereClause_fail =
  [] @=? (readP_to_S (parseWhereClause ' ') "WHERE age>27, name='Zoltan' ORDER BY name;")

test_read_WHERE :: Assertion
test_read_WHERE = assertRead (Just WHERE) "WHERE"

test_read_HAVING :: Assertion
test_read_HAVING = assertRead (Just HAVING) "HAVING"

test_read_GROUPBY :: Assertion
test_read_GROUPBY = assertRead (Just GROUPBY) "GROUP BY"

test_read_ORDERBY :: Assertion
test_read_ORDERBY = assertRead (Just ORDERBY) "ORDER BY"

test_read_invalid :: Assertion
test_read_invalid = assertRead (Nothing :: Maybe SQLClauseType) "INVALID"

test_showClause :: Assertion
test_showClause = "WHERE name='Zoltan', age>27" @=? showClause (WHERE, ["name='Zoltan'", "age>27"])
