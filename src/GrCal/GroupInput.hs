module GroupInput where

import qualified Data.Array as A
import qualified Data.Set as S
import Parsing
import Types (Group)
import Text.ParserCombinators.Parsec

data InputError = GroupError String | InputParseError ParseError

-- |This is the API-level function that takes a group table (in the form of
-- a slurped string) and produces either an error or the Group.
readGroup :: String -> Either InputError Table
readGroup input =
  case parseTable input of
    Left parseErrors -> Left $ InputParseError parseErrors
    Right parsedTable -> convertTable parsedTable

-- |This function takes a Table and produces either a parse/group error or
-- a well-defined Group
convertTable :: Table -> Either InputError Group
convertTable t = undefined

-- |This function takes a table an makes sure that the elements enumerated
-- in the first line are unique.
checkUniqueness :: Table -> Either InputError Bool
checkUniqueness Table t _ =
  -- Find the size of the first line and the size of unique elements in the
  -- line using a set.
  let firstLine = map string (head t)
      lineSet = S.fromList firstLine in
      if length firstLine == S.size lineSet then
        Right True
      else
        Left GroupError "Duplicate group elements declared in table"

-- |This function recursively ensures that the lines in the table are long
-- enough to have defined a correct operation.
checkLengths :: Table -> Int -> Either InputError Bool
checkLengths (Table _ []) n = Right True
checkLengths (Table _ [x]) n =
  if length x == n then
    Right True
  else
    Left GroupError "Line too short to define proper operation"
checkLengths (Table _ (x:xs)) n =
  if length x == n then
    case checkLengths xs n of
      Left e -> Left e
      Right b -> Right b
  else
    Left GroupError "Line too short to define proper operation"

-- |This function takes an index for the 2d array and checks to make sure
-- that the row and column of that index have unique elements
checkIndex :: (A.Ix a, A.Ix b) => A.Array (a,b) d -> a -> a -> Bool
checkIndex (a,b) arr index n =
  let columnSet = S.fromList [arr A.! (a,k) | k <- A.range (1, n)]
      rowSet = S.fromList [arr A.! (k,b) | k <- A.range (1, n)] in
      (S.size columnSet == n) and (S.size rowSet == n)

-- |This function checks that the operation is properly defined (i.e.
-- "sudoku rules")
checkOperation :: Table -> Either InputError Bool
checkOperation Table t ts =
  let s = S.fromList t
      n = S.size s
      a = A.listArray ((1, 1), (n,n)) ts in
      if foldl (\ t (i,j) -> t and checkIndex a (i,j) n) True [(x,x) | x <- A.range(1,n)] then
        Right True
      else
        GroupError "Operation violates \"sudoku rules\""

