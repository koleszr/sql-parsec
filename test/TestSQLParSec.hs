module TestSQLParSec
  ( tests
  ) where

import SQLParSec
import Text.ParserCombinators.ReadP
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

tests =
  [ testGroup_parseSQLCommand_select
  , testGroup_parseSQLCommand_update
  , testGroup_parseSQLCommand_delete
  , testGroup_parseClause
  , testGroup_parseClauses
  , testGroup_parseWord
  , testGroup_parseCommaSeparatedFields
  ]

testGroup_parseSQLCommand_select =
  testGroup
    "parseSQLCommand SELECT"
    [ testCase
        "Should parse SELECT command correctly"
        test_parseSQLCommand_select_upper
    , testCase
        "Should parse select command correctly"
        test_parseSQLCommand_select_lower
    , testCase
        "Should parse SELECT command with multiple clauses correctly"
        test_parseSQLCommand_select
    ]

testGroup_parseSQLCommand_update =
  testGroup
    "parseSQLCommand UPDATE"
    [ testCase
        "Should parse UPDATE command correclty"
        test_parseSQLCommand_update_upper
    , testCase
        "Should parse update command correctly"
        test_parseSQLCommand_update_lower
    , testCase
        "Should parse UPDATE command with multiple clauses correctly"
        test_parseSQLCommand_update
    ]

testGroup_parseSQLCommand_delete =
  testGroup
    "parseSQLCommand DELETE"
    [ testCase
        "Should parse DELETE command correclty"
        test_parseSQLCommand_delete_upper
    , testCase
        "Should parse delete command correctly"
        test_parseSQLCommand_delete_lower
    , testCase
        "Should parse DELETE command with where clause correctly"
        test_parseSQLCommand_delete
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

testGroup_parseWord =
  testGroup
    "parseWord"
    [ testCase "Should parse a word correctly with eof" test_parseWord_eof
    , testCase
        "Should parse a word correctly with trailing semicolon"
        test_parseWord_semicolon
    ]

testGroup_parseCommaSeparatedFields =
  testGroup
    "parseCommaSeparatedFields"
    [ testCase
        "Should parse comma separated fields correctly with single field"
        test_parseCommaSeparatedFields_single
    , testCase
        "Should parse comma separate fields correctly with trailing semicolon"
        test_parseCommaSeparatedFields_semicolon
    ]

test_parseCommaSeparatedFields_single :: Assertion
test_parseCommaSeparatedFields_single =
  assertParse (["*"], ()) (parseCommaSeparatedFields $ eof) "*"

test_parseCommaSeparatedFields_semicolon :: Assertion
test_parseCommaSeparatedFields_semicolon =
  assertParse
    (["age", "name"], ';')
    (parseCommaSeparatedFields $ satisfy (== ';'))
    "age, name;"

test_parseWord_eof :: Assertion
test_parseWord_eof = assertParse ("person", ()) (parseWord $ eof) "person"

test_parseWord_semicolon :: Assertion
test_parseWord_semicolon =
  assertParse ("person", ';') (parseWord $ (satisfy (== ';'))) "person;"

test_parseSQLCommand_select_upper :: Assertion
test_parseSQLCommand_select_upper =
  assertParse
    (SQLCommand SELECT "person" ["age", "name"] [])
    parseSQLCommand
    "SELECT age, name FROM person;"

test_parseSQLCommand_select_lower :: Assertion
test_parseSQLCommand_select_lower =
  assertParse
    (SQLCommand SELECT "person" ["age", "name"] [])
    parseSQLCommand
    "select age, name from person;"

test_parseSQLCommand_select :: Assertion
test_parseSQLCommand_select =
  assertParse
    (SQLCommand
       SELECT
       "person"
       ["age", "name"]
       [ (WHERE, ["age>27", "name='Zoltan'"])
       , (HAVING, ["COUNT(*)"])
       , (ORDERBY, ["name", "age"])
       ])
    parseSQLCommand
    "SELECT age, name FROM person WHERE age>27, name='Zoltan' HAVING COUNT(*) ORDERBY name, age;"

test_parseSQLCommand_update_upper :: Assertion
test_parseSQLCommand_update_upper =
  assertParse
    (SQLCommand UPDATE "person" ["age=30"] [])
    parseSQLCommand
    "UPDATE person SET age=30;"

test_parseSQLCommand_update_lower :: Assertion
test_parseSQLCommand_update_lower =
  assertParse
    (SQLCommand UPDATE "person" ["age=30"] [])
    parseSQLCommand
    "update person set age=30;"

test_parseSQLCommand_update :: Assertion
test_parseSQLCommand_update =
  assertParse
    (SQLCommand UPDATE "person" ["age=27"] [(WHERE, ["name='Zoltan'"])])
    parseSQLCommand
    "UPDATE person SET age=27 WHERE name='Zoltan';"

test_parseSQLCommand_delete_upper :: Assertion
test_parseSQLCommand_delete_upper =
  assertParse
    (SQLCommand DELETE "person" [] [])
    parseSQLCommand
    "DELETE FROM person;"

test_parseSQLCommand_delete_lower :: Assertion
test_parseSQLCommand_delete_lower =
  assertParse
    (SQLCommand DELETE "person" [] [])
    parseSQLCommand
    "DELETE FROM person;"

test_parseSQLCommand_delete :: Assertion
test_parseSQLCommand_delete =
  assertParse
    (SQLCommand DELETE "person" [] [(WHERE, ["name='Zoltan'"])])
    parseSQLCommand
    "DELETE FROM person WHERE name='Zoltan';"

test_parseClause_single :: Assertion
test_parseClause_single =
  assertParse
    (WHERE, ["age>27", "name='Zoltan'"], ';')
    (parseClause $ satisfy (== ';'))
    "WHERE age>27, name='Zoltan';"

test_parseClause_multiple :: Assertion
test_parseClause_multiple =
  assertParse
    (WHERE, ["age>27"], ';')
    (parseClause $ satisfy (== ';'))
    "WHERE age>27;"

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
    "WHERE age>27, name='Zoltan' HAVING COUNT(*) ORDERBY name, age;"

test_parseClauses_noSpace :: Assertion
test_parseClauses_noSpace =
  assertParse [] (parseClauses ';') "WHERE age>27;"

assertParse :: (Eq a, Show a) => a -> ReadP a -> String -> Assertion
assertParse expected parser str =
  case readP_to_S parser str of
    [(actual, _)] -> expected @=? actual
    otherwise -> assertFailure $ "Failure: " ++ show otherwise
