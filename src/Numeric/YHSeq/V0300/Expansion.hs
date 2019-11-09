module Numeric.YHSeq.V0300.Expansion
  ( expand
  ) where

  import Prelude hiding (length)

  import Numeric.YHSeq.V0300.Type
  import Numeric.YHSeq.V0300.Compression (seqClass, compress)
  import Numeric.YHSeq.V0300.Expansion.Class1 (reRoot1, expand1)
  import Numeric.YHSeq.V0300.Expansion.ClassN (reRootN, expandN)

  reRoot :: DPN -> Class -> Index
  reRoot z c = case c `compare` 1 of
    LT -> error "reRoot: non-positive class"
    EQ -> reRoot1 z
    GT -> reRootN z $ \s -> reRoot (compress s) (seqClass s)

  expand :: DPN -> Class -> Integer -> DPN
  expand z c n = case c `compare` 1 of
    LT -> error "expand: non-positive class"
    EQ -> expand1 z n
    GT -> expandN z n
