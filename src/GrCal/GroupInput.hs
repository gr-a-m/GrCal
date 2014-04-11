module GrCal.GroupInput where

import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GrCal.Parsing
import GrCal.Group
import Text.ParserCombinators.Parsec

data InputError = GroupError String | InputParseError ParseError deriving Show
newtype InputErrors = InputErrors [InputError] deriving Show

-- |This is the API-level function that takes a group table (in the form of
-- a slurped string) and produces either an error or the Group.
readGroup :: String -> Either InputErrors Group
readGroup input =
  case parseTable input of
    Left parseErrors -> Left $ InputErrors [InputParseError parseErrors]
    Right parsedTable ->
      case checkAll parsedTable of
        Left groupErrors -> Left groupErrors
        Right _ -> Right $ convertTable parsedTable

-- |Convert a 2D Map based on a list of ordered elements and the associated
-- operation matrix.
buildMap :: (Ord a) => [a] -> [[a]] -> M.Map (a, a) a
buildMap elems mat =
  let
    zipped = zip elems mat
  in
    foldl (\ a b -> M.union a $ buildPairs (fst b) elems (snd b)) M.empty zipped

-- |Helper for buildMap
buildPairs :: (Ord a) => a -> [a] -> [a] -> M.Map (a, a) a
buildPairs x elems line =
  let
    zipped = zip elems line
  in
    foldl (\ a b -> M.insert (x, fst b) (snd b) a) M.empty zipped

opToElem :: [TableLine] -> [[Element]]
opToElem = map (map (Element . unelems ) . elementList)

-- |This function takes a valid table and produces a Group structure
convertTable :: Table -> Group
convertTable t =
  let
    tableElements = elementList $ elementRow t
    members = map (Element . unelems) tableElements
    operationMap = buildMap members $ opToElem $ operationRows t
  in
    Group (S.fromList members) operationMap

-- |This is used by checkAll to convert a list of eithers to just the left
-- values.
leftOrNone :: Either a b -> [a]
leftOrNone e = case e of
  Right m -> []
  Left m -> [m]

-- |This function simply runs all of the other checks and either returns
-- a True value or the list of errors encountered.
checkAll :: Table -> Either InputErrors Bool
checkAll t =
  let
    unique = checkUniqueness t
    lengths = checkLengths t (length $ elementList $ elementRow t)
    operationCheck = checkOperation t
    errors = foldl (\a b -> a ++ leftOrNone b) [] [unique, lengths, operationCheck]
  in
    if null errors then
      Right True
    else
      Left $ InputErrors errors

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

checkAssociative :: Table -> Bool
checkAssociative t =
  let
    op = operationArray $ operationRows t
    pairs = [(x, y) | x <- [1 .. (length $ operationRows t)], y <- [1 .. (length $ operationRows t)]]
  in
    foldl (\ prev pair -> prev && (A.! op pair == A.! op (snd pair, fst pair))) False pairs


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

