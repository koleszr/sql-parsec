module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.HUnit
import TestSQLClause as Clause (tests)
import TestSQLCommand as Command (tests)
import TestSQLParSecUtils as Utils (tests)

main = defaultMain $ Clause.tests ++ Command.tests ++ Utils.tests
