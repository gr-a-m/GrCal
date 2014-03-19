module GrCal.GroupInput where

import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Set as S
import GrCal.Parsing
import GrCal.Types (Group)
import Text.ParserCombinators.Parsec

data InputError = GroupError String | InputParseError ParseError

-- |This is the API-level function that takes a group table (in the form of
-- a slurped string) and produces either an error or the Group.
readGroup :: String -> Either InputError Group
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
checkUniqueness (Table t _) =
  -- Find the size of the first line and the size of unique elements in the
  -- line using a set.
  let
    firstLine = elementList t
    lineSet = S.fromList firstLine
  in
    if length firstLine == S.size lineSet then
      Right True
    else
      Left $ GroupError "Duplicate group elements declared in table"

-- |This function recursively ensures that the lines in the table are long
-- enough to have defined a correct operation.
checkLengths :: Table -> Int -> Either InputError Bool
checkLengths (Table _ []) n = Right True
checkLengths (Table _ [x]) n =
  if length (elementList x) == n then
    Right True
  else
    Left $ GroupError "Line too short to define proper operation"
checkLengths (Table e (x:xs)) n =
  if length (elementList x) == n then
    case checkLengths (Table e xs) n of
      Left e -> Left e
      Right b -> Right b
  else
    Left $ GroupError "Line too short to define proper operation"

-- |This function takes an index for the 2d array and checks to make sure
-- that the row and column of that index have unique elements
checkIndex :: (Ord a) => A.Array (Int, Int) a -> (Int, Int) -> Int -> Bool
checkIndex arr (a,b) n =
  let
    columnSet = S.fromList [arr A.! (a,k) | k <- A.range (1, n)]
    rowSet = S.fromList [arr A.! (k,b) | k <- A.range (1, n)]
  in
    (n == S.size columnSet) && (S.size rowSet == n)

-- |Convert a table's operations into a matrix of elements
operationArray :: [TableLine] -> A.Array (Int, Int) Elem
operationArray lines =
  let
    twod = [elementList x | x <- lines]
    n = length twod
  in
    A.array ((1, 1), (n, n)) (listMat twod)

-- |Convert a 2d list into a 2d array with Int indices
listMat :: [[a]] -> [((Int, Int), a)]
listMat elements =
  concatMap (uncurry listMat') (zip [1..(length elements)] elements)

-- |Helper for listMat
listMat' :: Int -> [a] -> [((Int, Int), a)]
listMat' n ls =
  [((n, x), a) | (a, x) <- zip ls [1..(length ls)]]

-- |This function checks that the operation is properly defined (i.e.
-- "sudoku rules")
checkOperation :: Table -> Either InputError Bool
checkOperation t =
  let
    s = S.fromList $ elementList $ elementRow t
    n = S.size s
    a = operationArray $ operationRows t
    checkInd = [checkIndex a (x, x) n | x <- A.range(1,n)]
  in
    if L.and checkInd then
      Right True
    else
      Left $ GroupError "Operation violates \"sudoku rules\""

