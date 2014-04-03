module GrCal.Experiments where

import Data.List(foldl')
import qualified Data.Map as M

import GrCal.Parsing
import GrCal.Group (cyclic)
import GrCal.GroupInput (checkAll, convertTable, InputErrors)

-- |This function takes the parameters for the indirect product group and
-- the list of pre-computed elements and makes a 2d list of the group
-- operation table.
ipMap :: Int -> Int -> Int -> [(Int, Int)] -> [[(Int, Int)]]
ipMap n m a elems =
  let
    groupOp = indirectProduct n m a
  in
    [[groupOp x y | y <- elems] | x <- elems]

-- |Produce a table for the possible group of Z_n x Z_m with the indirect
-- product as the group law.
indirectProductTable :: Int -> Int -> Int -> Table
indirectProductTable n m a =
  if a > n then
    indirectProductTable n m (a `mod` n)
  else
    let
      groupOp = indirectProduct n m a
      elements = [(u, v) | u <- [0 .. (n - 1)], v <- [0 .. (m - 1)]]
      elementRow = TableLine $ map (makeElem . show) elements
      operationRows = map (TableLine . map (makeElem . show)) $ ipMap n m a elements
    in
      Table elementRow operationRows

-- |This function takes the groups of the coordinates, the value of a, and
-- two points and computes the indirect product with this information.
indirectProduct :: Int -> Int -> Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
indirectProduct n m a (u, v) (w, y) = ((u + w * a^y) `mod` n, (v + y) `mod` m)

validMap :: Int -> Int -> M.Map (Int, Int, Int) Bool
validMap maxN maxM =
  let
    params = [(n, m, a) | n <- [1 .. maxN], m <- [1 .. maxM], a <- [2 .. (n - 1)], gcd n a == 1]
  in
    foldl' insertEither M.empty params

insertEither :: M.Map (Int, Int, Int) Bool -> (Int, Int, Int) -> M.Map (Int, Int, Int) Bool
insertEither ma (n, m, a) =
  case checkAll $ indirectProductTable n m a of
    Left ie -> M.insert (n, m, a) False ma
    Right _ -> ma

checkZStar :: Int -> [Int]
checkZStar n =
  filter (cyclic . convertTable . zStarTable) [2 .. n]

zStarOp :: Int -> [Int] -> [[Int]]
zStarOp n elems =
  [[(x * y) `mod` n | y <- elems] | x <- elems]

zStarTable :: Int -> Table
zStarTable n =
  let
    elements = [a | a <- [1 .. (n - 1)], gcd a n == 1]
    elementRow = TableLine $ map (makeElem . show) elements
    operationRows = map (TableLine . map (makeElem . show)) $ zStarOp n elements
  in
    Table elementRow operationRows

