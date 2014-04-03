module GrCal.Experiments where

import GrCal.Parsing

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

