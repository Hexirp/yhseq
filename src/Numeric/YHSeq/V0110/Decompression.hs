module Numeric.YHSeq.V0110.Decompression
  ( reD
  , reP
  , decompress
  ) where

  import Prelude hiding (length)

  import Numeric.YHSeq.V0110.Type

  reD :: DPN -> Index -> Depth -> Diff
  reD z x n = if x <= 0
    then error "reD: non-positive index"
    else case n `compare` indexN z x of
      LT -> 0
      EQ -> indexD z x
      GT -> reP z x (n + 1) + reD z (reP z x n) n

  reP :: DPN -> Index -> Depth -> Diff
  reP z x n = if x <= 0
    then error "reP: non-positive index"
    else if n > indexN z x
      then 0
      else idx (reverse $ indexP z x) (indexN z x - n)

  decompress :: DPN -> Seq
  decompress z = map (\x -> reD z x 1) (enumFromTo 1 (lengthDPN z))
