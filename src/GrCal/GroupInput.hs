module GrCal.GroupInput where

import Parsing
import Types (Group)
import Text.ParserCombinators.Parsec

data InputError = GroupError String | InputParseError ParseError

readGroup :: String -> Either InputError Table
readGroup input =
  case parseTable input of
    Left parseErrors -> Left $ InputParseError parseErrors
    Right parsedTable -> Right parsedTable

convertTable :: Table -> Either InputError Group
convertTable t = undefined

-- |This function takes a table an makes sure that the elements enumerated
-- in the first line are unique.
checkUniqueness :: Table -> Bool
checkUniqueness t =
  -- Find the size of the first line and the size of unique elements in the
  -- line using a set.
  let firstLine = map String (head t)
      lineSet = Set firstLine in
      length firstLine == size lineSet

