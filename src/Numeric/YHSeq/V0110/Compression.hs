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
    GT -> searchParentAnc s x n
