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

tests =
  [ testGroup_parseWord
  , testGroup_parseCommaSeparatedFields
  , testGoup_parseString
  , testGroup_parseNumber
  ]

testGroup_parseWord =
  testGroup
    "parseWord"
    [ testCase "Should parse a word with eof" test_parseWord_eof
    , testCase
        "Should parse a word with trailing semicolon"
        test_parseWord_semicolon
    ]

testGroup_parseCommaSeparatedFields =
  testGroup
    "parseCommaSeparatedFields"
    [ testCase
        "Should parse comma separated fields with single field"
        test_parseCommaSeparatedFields_single
    , testCase
        "Should parse comma separate fields with trailing semicolon"
        test_parseCommaSeparatedFields_semicolon
    ]

testGoup_parseString =
  testGroup
  "parseString"
  [ testCase "Should parse string between apostrophes" test_parseString_apostrophes
  , testCase "Should parse string between quotation marks" test_parseString_quotation_marks
  ]

testGroup_parseNumber =
  testGroup
    "parseNumber"
    [ testCase "Should parse integer" test_parseNumber_int
    , testCase "Should parse floating point number" test_parseNumber_floating
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
  
test_parseString_apostrophes :: Assertion
test_parseString_apostrophes =
  assertParse
    (TextValue "Zoltan Koleszar", ())
    (parseString eof)
    "'Zoltan Koleszar'"

test_parseString_quotation_marks :: Assertion
test_parseString_quotation_marks =
  assertParse
    (TextValue "Zoltan Koleszar", ())
    (parseString eof)
    "\"Zoltan Koleszar\""

test_parseNumber_int :: Assertion
test_parseNumber_int = assertParse (NumberValue "25", ()) (parseNumber eof) "25"

test_parseNumber_floating :: Assertion
test_parseNumber_floating = assertParse (NumberValue "3.14", ()) (parseNumber eof) "3.14"
