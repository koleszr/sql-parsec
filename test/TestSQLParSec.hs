module TestSQLParSec
  ( tests
  ) where

import SQLParSec
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

tests =
  [ testGroup_parseSQLCommand_select
  , testGroup_parseSQLCommand_update
  , testGroup_parseSQLCommand_delete
  , testGroup_parseSQLCommand_insert
  , testGroup_parseClause
  , testGroup_parseClauses
  , testGroup_parseWord
  , testGroup_parseCommaSeparatedFields
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
    (Select "person" [column "age", column "name"] [])
    parseSQLCommand
    "SELECT age, name FROM person;"

test_parseSQLCommand_select_lower :: Assertion
test_parseSQLCommand_select_lower =
  assertParse
    (Select "person" [column "age", column "name"] [])
    parseSQLCommand
    "select age, name from person;"

test_parseSQLCommand_select :: Assertion
test_parseSQLCommand_select =
  assertParse
    (Select
       "person"
       [column "age", column "name"]
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
    (Update "person" [column "age=30"] [])
    parseSQLCommand
    "UPDATE person SET age=30;"

test_parseSQLCommand_update_lower :: Assertion
test_parseSQLCommand_update_lower =
  assertParse
    (Update "person" [column "age=30"] [])
    parseSQLCommand
    "update person set age=30;"

test_parseSQLCommand_update :: Assertion
test_parseSQLCommand_update =
  assertParse
    (Update "person" [column "age=27"] [(WHERE, ["name='Zoltan'"])])
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
    (Insert
       "person"
       [columnWithValue "name" "'Zoltan'", columnWithValue "age" "27"])
    parseSQLCommand
    "INSERT INTO person (name, age) VALUES ('Zoltan', 27);"

test_parseSQLCommand_insert_lower :: Assertion
test_parseSQLCommand_insert_lower =
  assertParse
    (Insert
       "person"
       [columnWithValue "name" "'Zoltan'", columnWithValue "age" "27"])
    parseSQLCommand
    "insert into person (name, age) values ('Zoltan', 27);"

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

assertParse :: (Eq a, Show a) => a -> ReadP a -> String -> Assertion
assertParse expected parser str =
  case readP_to_S parser str of
    [(actual, _)] -> expected @=? actual
    otherwise -> assertFailure $ "Failure: " ++ show otherwise

test_show_select :: Assertion
test_show_select =
  assertShow
    "SELECT name, age, COUNT(id) FROM person WHERE age>27 GROUP BY employer HAVING COUNT(id) ORDER BY COUNT(id);"
    (Select
       "person"
       [column "name", column "age", column "COUNT(id)"]
       [ (WHERE, ["age>27"])
       , (GROUPBY, ["employer"])
       , (HAVING, ["COUNT(id)"])
       , (ORDERBY, ["COUNT(id)"])
       ])

test_show_select_without_clause :: Assertion
test_show_select_without_clause =
  assertShow
    "SELECT name, age FROM person;"
    (Select "person" [column "name", column "age"] [])

test_show_insert :: Assertion
test_show_insert =
  assertShow
    "INSERT INTO person (name, age, employer) VALUES ('Zoltan', 27, 'E Corp');"
    (Insert
       "person"
       [ columnWithValue "name" "'Zoltan'"
       , columnWithValue "age" "27"
       , columnWithValue "employer" "'E Corp'"
       ])

test_show_delete :: Assertion
test_show_delete =
  assertShow
    "DELETE FROM person WHERE age>27;"
    (Delete "person" [(WHERE, ["age>27"])])

test_show_update :: Assertion
test_show_update =
  assertShow
    "UPDATE person SET age=27 WHERE age=26;"
    (Update "person" [column "age=27"] [(WHERE, ["age=26"])])

test_show_update_without_clause :: Assertion
test_show_update_without_clause =
  assertShow "UPDATE person SET age=30;" (Update "person" [column "age=30"] [])

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
