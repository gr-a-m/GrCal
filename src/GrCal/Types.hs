module GrCal.Types where

import qualified Data.Map as M
import qualified Data.Set as S

newtype Element = Element String
data Group = Group (S.Set Element) (M.Map (Element, Element) Element)

