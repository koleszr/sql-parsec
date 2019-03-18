module Lib
  ( parse
  ) where

import SQLCommand
import Text.ParserCombinators.ReadP

parse :: String -> Maybe SQLCommand
parse str =
  case readP_to_S parseSQLCommand str of
    ((Just cmd, _):[]) -> Just cmd
    _ -> Nothing
