module Numeric.YHSeq.V0110.Compression where

  import Prelude
  import Numeric.YHSeq.V0110.Type

  mtD :: Seq -> Index -> Depth -> Diff
  mtD s x n = case n `compare` 1 of
    LT -> error "mtD: non-positive depth"
    EQ -> indexSeq s x
    GT -> case mtP s x (n - 1) `compare` 0 of
      LT -> error "mtD: irregular value of mtP"
      EQ -> 0
      GT -> mtD s x (n - 1) - mtD s (mtP s x (n - 1)) (n - 1)

  mtP :: Seq -> Index -> Depth -> ParentIndex
  mtP s x n = case n `compare` 1 of
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
  searchParent s x = searchParent' s x (x - 1)

  searchParent' :: Seq -> Index -> ParentIndex -> ParentIndex
  searchParent' s x p = if p <= 0
    then error "searchParent: no parent"
    else if mtD s p n < mtD s x n
      then p
      else searchParent' s x (p - 1)

  searchParentAnc :: Seq -> Index -> Depth -> ParentIndex
  searcnParentAnc s x n = case n `compare` 1 of
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
