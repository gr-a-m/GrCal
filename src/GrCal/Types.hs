module GrCal.Types where

import qualified Data.Map as M
import qualified Data.Set as S

-- |An element is just some identifying string
newtype Element = Element String
-- |A group is a set of elements and the operation that maps a pair of
-- elements to another element.
data Group = Group {
  members :: S.Set Element,
  groupOperation :: M.Map (Element, Element) Element
}

