module TestSQLParSecUtils
  ( tests
  ) where

import SQLParSecUtils
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestUtils

tests = [testGroup_parseWord, testGroup_parseCommaSeparatedFields]

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
  
