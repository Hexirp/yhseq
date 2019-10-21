module Numeric.YHSeq.V0110.Compression
  ( mtD
  , mtP
  , searchParent
  , searchParentAnc
  , anc
  ) where

  import Prelude
  import Numeric.YHSeq.V0110.Type

  mtD :: Seq -> Index -> Depth -> Diff
  mtD s x n = if x <= 0
    then error "mtD: non-positive index"
    else case n `compare` 1 of
      LT -> error "mtD: non-positive depth"
      EQ -> indexSeq s x
      GT -> case mtP s x (n - 1) `compare` 0 of
        LT -> error "mtD: irregular value of mtP"
        EQ -> 0
        GT -> mtD s x (n - 1) - mtD s (mtP s x (n - 1)) (n - 1)

  mtP :: Seq -> Index -> Depth -> ParentIndex
  mtP s x n = if x <= 0
    then error "mtP: non-positive index"
    else case n `compare` 1 of
      LT -> error "mtP: non-positive depth"
      EQ -> case mtD s x n `compare` 1 of
        LT -> error "mtP: irregular value of mtD"
        EQ -> 0
        GT -> searchParent s x
      GT -> case mtD s x n `compare` 0 of
        LT -> error "mtP: irregular value of mtD"
        EQ -> 0
        GT -> searchParentAnc s x n

  searchParent :: Seq -> Index -> ParentIndex
  searchParent s x = if x <= 0
    then error "searchParent: non-positive index"
    else searchParent' s x (x - 1)

  searchParent' :: Seq -> Index -> ParentIndex -> ParentIndex
  searchParent' s x p = case p `compare` 0 of
    LT -> error "searchParent: impossible case"
    EQ -> error "searchParent: no parent"
    GT -> if mtD s p 1 < mtD s x 1
      then p
      else searchParent' s x (p - 1)

  searchParentAnc :: Seq -> Index -> Depth -> ParentIndex
  searchParentAnc s x n = if x <= 0
    then error "searchParentAnc: non-positive index"
    else case n `compare` 1 of
      LT -> error "searchParentAnc: non-positive depth"
      EQ -> error "searchParentAnc: use searchParent when depth is 1"
      GT -> searchParentAnc' s x n x

  searchParentAnc' :: Seq -> Index -> Depth -> ParentIndex -> ParentIndex
  searchParentAnc' s x n p = case p `compare` 0 of
    LT -> error "searchParentAnc: impossible case"
    EQ -> error "searchParentAnc: no parent"
    GT -> if mtD s p n < mtD s x n && p `elem` anc s x (n - 1)
      then p
      else searchParentAnc' s x n (p - 1)

  anc :: Seq -> Index -> Depth -> [ParentIndex]
  anc s x n = if x <= 0
    then error "anc: non-positive index"
    else if n <= 0
      then error "anc: non-positive depth"
      else anc' s x n

  anc' :: Seq -> Index -> Depth -> [ParentIndex]
  anc' s x n = case x `compare` 0 of
    LT -> error "anc: irregular value of mtP"
    EQ -> []
    GT -> x : anc' s (mtP s x n) n
