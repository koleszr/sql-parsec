module TestSQLCommand (tests) where

import SQLClause
import SQLCommand
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestUtils
import Text.Read (readMaybe)

tests =
  [ testGroup_parseSQLCommand_select
  , testGroup_parseSQLCommand_update
  , testGroup_parseSQLCommand_delete
  , testGroup_parseSQLCommand_insert
  , testGroup_show
  , testGroup_read_SQLCommandType
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
        "Should parse UPDATE command correctly"
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
    "parseSQLCommand DELETE FROM"
    [ testCase
        "Should parse DELETE command correctly"
        test_parseSQLCommand_delete_upper
    , testCase
        "Should parse delete command correctly"
        test_parseSQLCommand_delete_lower
    , testCase
        "Should parse DELETE command with where clause correctly"
        test_parseSQLCommand_delete
    ]

testGroup_parseSQLCommand_insert =
  testGroup
    "parseSqlcommand INSERT INTO"
    [ testCase
        "Should parse INSERT command correctly"
        test_parseSQLCommand_insert_upper
    , testCase
        "Should parse insert command correctly"
        test_parseSQLCommand_insert_lower
    ]

testGroup_show =
  testGroup
    "show"
    [ testCase "Should show SELECT correctly" test_show_select
    , testCase
        "Should show SELECT without clauses correcty"
        test_show_select_without_clause
    , testCase "Should show INSERT correctly" test_show_insert
    , testCase "Should show DELETE correctly" test_show_delete
    , testCase "Should show UPDATE correctly" test_show_update
    , testCase
        "Should show UPDATE without clauses correctly"
        test_show_update_without_clause
    ]

testGroup_read_SQLCommandType =
  testGroup
    "read SQLCommandType"
    [ testCase
        "Should read \"SELECT\" or \"select\" as SELECT"
        test_read_SQLCommandType_select
    , testCase
        "Should read \"UPDATE\" or \"update\" as UPDATE"
        test_read_SQLCommandType_update
    , testCase
        "Should read \"DELETE\" or \"delete\" as DELETE"
        test_read_SQLCommandType_delete
    , testCase
        "Should read \"INSERT\" or \"insert\" as INSERT"
        test_read_SQLCommandType_insert
    , testCase "Should fail on invalid input" test_read_SQLCommandType_invalid
    ]    

test_parseSQLCommand_select_upper :: Assertion
test_parseSQLCommand_select_upper =
  assertParse
    (Select "person" ["age", "name"] [])
    parseSQLCommand
    "SELECT age, name FROM person;"

test_parseSQLCommand_select_lower :: Assertion
test_parseSQLCommand_select_lower =
  assertParse
    (Select "person" ["age", "name"] [])
    parseSQLCommand
    "select age, name from person;"

test_parseSQLCommand_select :: Assertion
test_parseSQLCommand_select =
  assertParse
    (Select
       "person"
       ["age", "name"]
       [ (WHERE, ["age>27", "name='Zoltan'"])
       , (HAVING, ["COUNT(*)"])
       , (GROUPBY, ["employer"])
       , (ORDERBY, ["name", "age"])
       ])
    parseSQLCommand
    "SELECT age, name FROM person WHERE age>27, name='Zoltan' HAVING COUNT(*) GROUP BY employer ORDER BY name, age;"

test_parseSQLCommand_update_upper :: Assertion
test_parseSQLCommand_update_upper =
  assertParse
    (Update "person" ["age=30"] [])
    parseSQLCommand
    "UPDATE person SET age=30;"

test_parseSQLCommand_update_lower :: Assertion
test_parseSQLCommand_update_lower =
  assertParse
    (Update "person" ["age=30"] [])
    parseSQLCommand
    "update person set age=30;"

test_parseSQLCommand_update :: Assertion
test_parseSQLCommand_update =
  assertParse
    (Update "person" ["age=27"] [(WHERE, ["name='Zoltan'"])])
    parseSQLCommand
    "UPDATE person SET age=27 WHERE name='Zoltan';"

test_parseSQLCommand_delete_upper :: Assertion
test_parseSQLCommand_delete_upper =
  assertParse (Delete "person" []) parseSQLCommand "DELETE FROM person;"

test_parseSQLCommand_delete_lower :: Assertion
test_parseSQLCommand_delete_lower =
  assertParse (Delete "person" []) parseSQLCommand "DELETE FROM person;"

test_parseSQLCommand_delete :: Assertion
test_parseSQLCommand_delete =
  assertParse
    (Delete "person" [(WHERE, ["name='Zoltan'"])])
    parseSQLCommand
    "DELETE FROM person WHERE name='Zoltan';"

test_parseSQLCommand_insert_upper :: Assertion
test_parseSQLCommand_insert_upper =
  assertParse
    (Insert "person" ["name", "age"] ["'Zoltan'", "27"])
    parseSQLCommand
    "INSERT INTO person (name, age) VALUES ('Zoltan', 27);"

test_parseSQLCommand_insert_lower :: Assertion
test_parseSQLCommand_insert_lower =
  assertParse
    (Insert "person" ["name", "age"] ["'Zoltan'", "27"])
    parseSQLCommand
    "insert into person (name, age) values ('Zoltan', 27);"

test_show_select :: Assertion
test_show_select =
  assertShow
    "SELECT name, age, COUNT(id) FROM person WHERE age>27 GROUP BY employer HAVING COUNT(id) ORDER BY COUNT(id);"
    (Select
       "person"
       ["name", "age", "COUNT(id)"]
       [ (WHERE, ["age>27"])
       , (GROUPBY, ["employer"])
       , (HAVING, ["COUNT(id)"])
       , (ORDERBY, ["COUNT(id)"])
       ])

test_show_select_without_clause :: Assertion
test_show_select_without_clause =
  assertShow
    "SELECT * FROM person;"
    (Select "person" ["*"] [])

test_show_insert :: Assertion
test_show_insert =
  assertShow
    "INSERT INTO person (name, age, employer) VALUES ('Zoltan', 27, 'E Corp');"
    (Insert "person" ["name", "age", "employer"] ["'Zoltan'", "27", "'E Corp'"])

test_show_delete :: Assertion
test_show_delete =
  assertShow
    "DELETE FROM person WHERE age>27;"
    (Delete "person" [(WHERE, ["age>27"])])

test_show_update :: Assertion
test_show_update =
  assertShow
    "UPDATE person SET age=27, name='Zoli' WHERE age=26, name='Zoltan';"
    (Update "person" ["age=27", "name='Zoli'"] [(WHERE, ["age=26", "name='Zoltan'"])])

test_show_update_without_clause :: Assertion
test_show_update_without_clause =
  assertShow "UPDATE person SET age=30;" (Update "person" ["age=30"] [])

assertShow :: String -> SQLCommand -> Assertion
assertShow expected cmd = expected @=? show cmd

test_read_SQLCommandType_select :: Assertion
test_read_SQLCommandType_select =
  ((Just SELECT) @=? (readMaybe "SELECT")) >>
  ((Just SELECT) @=? (readMaybe "select"))

test_read_SQLCommandType_update =
  ((Just UPDATE) @=? (readMaybe "UPDATE")) >>
  ((Just UPDATE) @=? (readMaybe "update"))

test_read_SQLCommandType_delete :: Assertion
test_read_SQLCommandType_delete =
  ((Just DELETE) @=? (readMaybe "DELETE")) >>
  ((Just DELETE) @=? (readMaybe "delete"))

test_read_SQLCommandType_insert :: Assertion
test_read_SQLCommandType_insert =
  ((Just INSERT) @=? (readMaybe "INSERT")) >>
  ((Just INSERT) @=? (readMaybe "insert"))

test_read_SQLCommandType_invalid :: Assertion
test_read_SQLCommandType_invalid =
  Nothing @=? ((readMaybe "invalid") :: Maybe SQLCommandType)
