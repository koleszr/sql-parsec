module TestSQLCommand (tests) where

import SQLClause
import SQLCommand
import SQLCondition
import SQLConditionCombinator
import SQLParSecUtils (SQLValue (..))
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestUtils
import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP

tests =
  [ testGroup_parseSQLCommand_select
  , testGroup_parseSQLCommand_update
  , testGroup_parseSQLCommand_delete
  , testGroup_parseSQLCommand_insert
  , testGroup_parseSQLCommand_fail
  , testGroup_show
  ]

testGroup_parseSQLCommand_select =
  testGroup
    "parseSQLCommand SELECT"
    [ testCase
        "Should parse SELECT command"
        test_parseSQLCommand_select_upper
    , testCase
        "Should parse select command"
        test_parseSQLCommand_select_lower
    , testCase
        "Should parse SELECT command with multiple clauses"
        test_parseSQLCommand_select
    ]

testGroup_parseSQLCommand_update =
  testGroup
    "parseSQLCommand UPDATE"
    [ testCase
        "Should parse UPDATE command"
        test_parseSQLCommand_update_upper
    , testCase
        "Should parse update command"
        test_parseSQLCommand_update_lower
    , testCase
        "Should parse UPDATE command with multiple clauses"
        test_parseSQLCommand_update
    ]

testGroup_parseSQLCommand_delete =
  testGroup
    "parseSQLCommand DELETE FROM"
    [ testCase
        "Should parse DELETE command"
        test_parseSQLCommand_delete_upper
    , testCase
        "Should parse delete command"
        test_parseSQLCommand_delete_lower
    , testCase
        "Should parse DELETE command with where clause"
        test_parseSQLCommand_delete
    ]

testGroup_parseSQLCommand_insert =
  testGroup
    "parseSqlcommand INSERT INTO"
    [ testCase
        "Should parse INSERT command"
        test_parseSQLCommand_insert_upper
    , testCase
        "Should parse insert command"
        test_parseSQLCommand_insert_lower
    ]

testGroup_parseSQLCommand_fail =
  testGroup
  "parseSQLCommand INVALID"
  [testCase "Should fail if invalid command was given" test_parseSQLCommand_fail_on_invalid]

test_parseSQLCommand_fail_on_invalid :: Assertion
test_parseSQLCommand_fail_on_invalid =
  [(Nothing, "")] @=? (readP_to_S parseSQLCommand "INVALID ")
  

testGroup_show =
  testGroup
    "show"
    [ testCase "Should show SELECT" test_show_select
    , testCase
        "Should show SELECT without clauses"
        test_show_select_without_clause
    , testCase "Should show INSERT" test_show_insert
    , testCase "Should show DELETE" test_show_delete
    , testCase
        "Should show DELETE without clauses"
        test_show_delete_without_clause
    , testCase "Should show UPDATE" test_show_update
    , testCase
        "Should show UPDATE without clauses"
        test_show_update_without_clause
    ]

test_parseSQLCommand_select_upper :: Assertion
test_parseSQLCommand_select_upper =
  assertParse
    (Just $ Select "person" ["age", "name"] [])
    parseSQLCommand
    "SELECT age, name FROM person;"

test_parseSQLCommand_select_lower :: Assertion
test_parseSQLCommand_select_lower =
  assertParse
    (Just $ Select "person" ["age", "name"] [])
    parseSQLCommand
    "select age, name from person;"

test_parseSQLCommand_select :: Assertion
test_parseSQLCommand_select =
  assertParse
    (Just $ Select
       "person"
       ["age", "name", "COUNT(age)"]
       [ Where
           (Bin
              AND
              (Pure $ SQLCondition GRT "age" (NumberValue "27"))
              (Pure $ SQLCondition E "name" (TextValue "Zoltan")))
       , GroupBy ["age", "name"]
       , Having (Pure $ SQLCondition GTE "COUNT(age)" (NumberValue "10"))
       , OrderBy [SQLOrdering ["age"] DESC, SQLOrdering ["name"] ASC]
       ])
    parseSQLCommand
    "SELECT age, name, COUNT(age) FROM person WHERE age>27 AND name='Zoltan' GROUP BY age, name HAVING COUNT(age) >= 10 ORDER BY age DESC, name ASC;"

test_parseSQLCommand_update_upper :: Assertion
test_parseSQLCommand_update_upper =
  assertParse
    (Just $ Update "person" ["age=30"] Nothing)
    parseSQLCommand
    "UPDATE person SET age=30;"

test_parseSQLCommand_update_lower :: Assertion
test_parseSQLCommand_update_lower =
  assertParse
    (Just $ Update "person" ["age=30"] Nothing)
    parseSQLCommand
    "update person set age=30;"

test_parseSQLCommand_update :: Assertion
test_parseSQLCommand_update =
  assertParse
    (Just $ Update
       "person"
       ["age=27"]
       (Just (Where (Pure $ SQLCondition E "name" (TextValue "Zoltan")))))
    parseSQLCommand
    "UPDATE person SET age=27 WHERE name='Zoltan';"

test_parseSQLCommand_delete_upper :: Assertion
test_parseSQLCommand_delete_upper =
  assertParse (Just $ Delete "person" Nothing) parseSQLCommand "DELETE FROM person;"

test_parseSQLCommand_delete_lower :: Assertion
test_parseSQLCommand_delete_lower =
  assertParse (Just $ Delete "person" Nothing) parseSQLCommand "delete from person;"

test_parseSQLCommand_delete :: Assertion
test_parseSQLCommand_delete =
  assertParse
    (Just $ Delete "person" (Just (Where (Pure $ SQLCondition E "name" (TextValue "Zoltan")))))
    parseSQLCommand
    "DELETE FROM person WHERE name='Zoltan';"

test_parseSQLCommand_insert_upper :: Assertion
test_parseSQLCommand_insert_upper =
  assertParse
    (Just $ Insert "person" ["name", "age"] ["'Zoltan'", "27"])
    parseSQLCommand
    "INSERT INTO person (name, age) VALUES ('Zoltan', 27);"

test_parseSQLCommand_insert_lower :: Assertion
test_parseSQLCommand_insert_lower =
  assertParse
    (Just $ Insert "person" ["name", "age"] ["'Zoltan'", "27"])
    parseSQLCommand
    "insert into person (name, age) values ('Zoltan', 27);"

test_show_select :: Assertion
test_show_select =
  assertShow
    "SELECT employer, name, COUNT(employer) FROM person WHERE age > 27 GROUP BY employer, name HAVING COUNT(employer) < 500 ORDER BY employer, COUNT(employer) ASC, name DESC;"
    (Select
       "person"
       ["employer", "name", "COUNT(employer)"]
       [ Where (Pure $ SQLCondition GRT "age" (NumberValue "27"))
       , GroupBy ["employer", "name"]
       , Having (Pure $ SQLCondition LET "COUNT(employer)" (NumberValue "500"))
       , OrderBy [SQLOrdering ["employer", "COUNT(employer)"] ASC, SQLOrdering ["name"] DESC]
       ])

test_show_select_without_clause :: Assertion
test_show_select_without_clause =
  assertShow "SELECT * FROM person;" (Select "person" ["*"] [])

test_show_insert :: Assertion
test_show_insert =
  assertShow
    "INSERT INTO person (name, age, employer) VALUES ('Zoltan', 27, 'E Corp');"
    (Insert "person" ["name", "age", "employer"] ["'Zoltan'", "27", "'E Corp'"])

test_show_delete :: Assertion
test_show_delete =
  assertShow
    "DELETE FROM person WHERE age > 27 AND name = 'Zoltan';"
    (Delete
       "person"
       (Just
          (Where
             (Bin
                AND
                (Pure $ SQLCondition GRT "age" (NumberValue "27"))
                (Pure $ SQLCondition E "name" (TextValue "Zoltan"))))))

test_show_delete_without_clause :: Assertion
test_show_delete_without_clause =
  assertShow "DELETE FROM person;" (Delete "person" Nothing)

test_show_update :: Assertion
test_show_update =
  assertShow
    "UPDATE person SET age=27, name='Zoli' WHERE age = 26 AND name = 'Zoltan';"
    (Update
       "person"
       ["age=27", "name='Zoli'"]
       (Just
          (Where
             (Bin
                AND
                (Pure $ SQLCondition E "age" (NumberValue "26"))
                (Pure $ SQLCondition E "name" (TextValue "Zoltan"))))))

test_show_update_without_clause :: Assertion
test_show_update_without_clause =
  assertShow "UPDATE person SET age=30;" (Update "person" ["age=30"] Nothing)
