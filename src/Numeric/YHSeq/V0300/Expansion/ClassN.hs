module Numeric.YHSeq.V0300.Expansion.Class1
  ( badRoot
  , goodPart
  , badPart
  , cuttedPart
  , pnt
  , anc
  , delta
  , amt
  , bas
  , rising
  , rise
  , ris
  , newD
  , newP
  , newN
  , copiedBadPart
  , expand1
  ) where

  import Prelude hiding (length)

  import Numeric.YHSeq.V0300.Type

  badRoot :: DPN -> Index
  badRoot z = last $ indexP z (lengthDPN z)

  goodPart :: DPN -> DPN
  goodPart z = sliceDPN 1 (badRoot z - 1) z

  badPart :: DPN -> DPN
  badPart z = sliceDPN (badRoot z) (lengthDPN z - 1) z

  cuttedPart :: DPN -> DPN
  cuttedPart z = sliceDPN (lengthDPN z) (lengthDPN z) z


  pnt :: DPN -> Index -> Depth -> ParentIndex
  pnt z x n = if x <= 0
    then error "pnt: non-positive index"
    else if n > indexN z x
      then 0
      else idx (reverse $ indexP z x) (indexN z x - n)

  anc :: DPN -> Index -> Depth -> [ParentIndex]
  anc z x n = if x <= 0
    then error "anc: non-positive index"
    else if n <= 0
      then error "anc: non-positive depth"
      else anc' z x n

  anc' :: DPN -> Index -> Depth -> [ParentIndex]
  anc' z x n = case x `compare` 0 of
    LT -> error "anc: irregular value of pnt"
    EQ -> []
    GT -> x : anc' z (pnt z x n) n


  type NParentIndex = Index

  -- n (depth) + pnt (parent)
  npt :: DPN -> Index -> NParentIndex
  npt z x = if x <= 0
    then error "npt: non-postive index"
    else npt' z x (x - 1)

  npt' :: DPN -> Index -> NParentIndex -> NParentIndex
  npt' z x p = case p `compare` 0 of
    LT -> error "npt: impossible case"
    EQ -> 0
    GT -> if indexN z p < indexN z x
      then p
      else npt' z x (p - 1)
