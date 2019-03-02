module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestSQLParSec (tests)

main = defaultMain tests
