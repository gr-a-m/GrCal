module GrCal.Group where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

-- |An element is just some identifying string
newtype Element = Element String deriving (Show, Ord, Eq)
-- |A group is a set of elements and the operation that maps a pair of
-- elements to another element.
data Group = Group {
  members :: S.Set Element,
  groupOperation :: M.Map (Element, Element) Element
} deriving Show

-- |This function naively takes a group and determines if it is cyclic or
-- not.
cyclic :: Group -> Bool
cyclic g =
  any (\ e -> cyclic' g e e (S.size $ members g)) (S.elems $ members g)

-- |This helper function is used in the defininition of cyclic
cyclic' :: Group -> Element -> Element -> Int -> Bool
cyclic' _ _ _ 0 = False -- If past c^n, False (not possible w/ finite)
cyclic' g c e n = -- Next is c^{size - n + 2}
  let
    next = M.lookup (e, c) (groupOperation g)
    groupElements = members g
  in
    if maybe True (== c) next then -- If we found the generator before n, False
      S.size groupElements - n + 1 == S.size groupElements
    else
      cyclic' g c (fromMaybe e next) (n - 1)

-- |This function is used to get the identity of a group.
groupId :: Group -> Element
groupId = undefined

-- |This function takes an element g from the group G and computes g^n
groupPower :: Group -> Element -> Int -> Element
groupPower g _ 0 = groupId g
groupPower _ e 1 = e
groupPower g e n =
  M.findWithDefault (groupId g) (groupPower g e (quot n 2), groupPower g e (quot n 2)) (groupOperation g)

-- |This function naively checks if a group is abelian
abelian :: Group -> Bool
abelian g = all (uncurry (==)) $ commutePairs g

-- |This function converts a group into a list of pairs containing the
-- results of each pair of elements applied to eachother in both orders.
commutePairs :: Group -> [(Element, Element)]
commutePairs g =
  let
    elementSet = members g
    gop = groupOperation g
  in
    [(M.findWithDefault (Element "nil") (x,y) gop,
      M.findWithDefault (Element "lin") (y,x) gop) |
      x <- S.elems elementSet, y <- S.elems elementSet]

-- |Given a modulus and a list of integers, create a map from the elements
-- to their sums reduced by the modulus.
zMap :: Int -> [Int] -> M.Map (Element, Element) Element
zMap n mem = M.fromList [((Element $ show x, Element $ show y), Element $ show $ (x + y) `mod` n) | x <- mem, y <- mem]

-- |Build the group Z_n from the provided integer.
groupZ :: Int -> Group
groupZ n =
  let
    groupMembers = [0 .. (n - 1)]
    opm = zMap n groupMembers
  in
    Group (S.fromList $ map (Element . show) groupMembers) opm

