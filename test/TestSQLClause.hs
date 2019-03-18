module TestSQLClause (tests) where

import SQLClause
import SQLCondition
import SQLConditionCombinator
import SQLParSecUtils (SQLValue (..))
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestUtils
import Text.ParserCombinators.ReadP

tests =
  [ testGroup_parseSQLClauses
  , testGroup_parseSQLClauses_ORDER_BY
  , testGroup_parseWhereClause
  , testCase "Should show clause" test_show
  ]

testGroup_parseSQLClauses =
  testGroup
    "parseSQLClauses"
    [ testCase "Should parse single SQL clause" test_parseSQLClauses_single
    , testCase "Should parse multiple SQL clauses" test_parseSQLClauses_multiple
    ]

test_parseSQLClauses_single :: Assertion
test_parseSQLClauses_single =
  assertParse
    [Where (Pure $ SQLCondition E "age" (NumberValue "27"))]
    parseSQLClauses
    "WHERE age = 27;"

test_parseSQLClauses_multiple :: Assertion
test_parseSQLClauses_multiple =
  assertParse
    [ Where
        (Bin
           OR
           (Pure $ SQLCondition GRT "age" (NumberValue "27"))
           (Pure $ SQLCondition E "name" (TextValue "Zoltan Koleszar")))
    , GroupBy ["age"]
    , Having (Pure $ SQLCondition GRT "COUNT(age)" (NumberValue "25"))
    ]
    parseSQLClauses
    "WHERE age>27 OR name='Zoltan Koleszar' GROUP BY age HAVING COUNT(age) > 25;"

testGroup_parseSQLClauses_ORDER_BY =
  testGroup
    "ORDER BY with parseSQLClauses"
    [ testCase
        "Should parse single column without specifing order"
        test_ORDER_BY_single
    , testCase
        "Should parse multiple columns without specifing order"
        test_ORDER_BY_multiple
    , testCase
        "Should parse single column in ascending order"
        test_ORDER_BY_single_ASC
    , testCase
        "Should parse multiple columns in descending order"
        test_ORDER_BY_multiple_DESC
    , testCase "Should parse columns in ASC and DESC" test_ORDER_BY_mixed
    ]

test_ORDER_BY_single :: Assertion
test_ORDER_BY_single =
  assertParse
    [OrderBy [SQLOrdering ["name"] ASC]]
    parseSQLClauses
    "ORDER BY name;"

test_ORDER_BY_multiple :: Assertion
test_ORDER_BY_multiple =
  assertParse
    [OrderBy [SQLOrdering ["name", "age"] ASC]]
    parseSQLClauses
    "ORDER BY name, age;"

test_ORDER_BY_single_ASC :: Assertion
test_ORDER_BY_single_ASC =
  assertParse
    [OrderBy [SQLOrdering ["name"] ASC]]
    parseSQLClauses
    "ORDER BY name ASC;"

test_ORDER_BY_multiple_DESC :: Assertion
test_ORDER_BY_multiple_DESC =
  assertParse
    [OrderBy [SQLOrdering ["name", "age"] DESC]]
    parseSQLClauses
    "ORDER BY name, age DESC;"

test_ORDER_BY_mixed :: Assertion
test_ORDER_BY_mixed =
  assertParse
    [ OrderBy
        [ SQLOrdering ["name"] ASC
        , SQLOrdering ["age", "salary"] DESC
        ]
    ]
    parseSQLClauses
    "ORDER BY name ASC, age, salary DESC;"


testGroup_parseWhereClause =
  testGroup
    "parseWhereClause"
    [ testCase
        "Should return WHERE clause if it was the only one provided"
        test_parseWhereClause
    , testCase
        "Should fail if multiple clauses were provided"
        test_parseWhereClause_fail
    ]

test_parseWhereClause :: Assertion
test_parseWhereClause =
  assertParse
    (Right
       (Just
          (Where $
           Bin
             OR
             (Pure $ SQLCondition GRT "age" (NumberValue "27"))
             (Pure $ SQLCondition E "name" (TextValue "Zoltan")))))
    parseWhereClause
    "WHERE age>27 OR name='Zoltan' ORDER BY name;"

test_parseWhereClause_fail :: Assertion
test_parseWhereClause_fail =
  [(Left "Found more than one element!", "")] @=?
  (readP_to_S
     parseWhereClause
     "WHERE age>27 AND name='Zoltan' WHERE language = 'Haskell';")

test_parseSQLClause_single :: Assertion
test_parseSQLClause_single =
  assertParse
    [Where (Pure $ SQLCondition GRT "age" (NumberValue "27"))]
    parseSQLClauses
    "WHERE age>27;"

test_parseSQLClause_multiple :: Assertion
test_parseSQLClause_multiple =
  assertParse
    [ Where
        (Bin
           AND
           (Pure $ SQLCondition GRT "age" (NumberValue "27"))
           (Pure $ SQLCondition E "name" (TextValue "Zoltan")))
    ]
    parseSQLClauses
    "WHERE age>27 AND name='Zoltan';"

test_show :: Assertion
test_show =
  "WHERE name = 'Zoltan' OR age > 27" @=?
  show
    (Where
       (Bin
          OR
          (Pure $ SQLCondition E "name" (TextValue "Zoltan"))
          (Pure $ SQLCondition GRT "age" (NumberValue "27"))))
