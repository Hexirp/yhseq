module Numeric.YHSeq.V0110.Compression where

  import Prelude
  import Numeric.YHSeq.V0110.Type

  mtD :: Seq -> Index -> Depth -> Diff
  mtD s x n = case n `compare` 1 of
    LT -> error "mtD: not positive depth"
    EQ -> indexSeq s x
    GT -> case mtP s x (n - 1) `compare` 0 of
      LT -> error "mtD: irregular value of mtP"
      EQ -> 0
      GT -> mtD s x (n - 1) - mtD s (mtP s x (n - 1)) (n - 1)
