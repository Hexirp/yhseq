module Numeric.YHSeq.V0111.Decompression
  ( reD
  , reP
  , decompress
  ) where

  import Prelude hiding (length)

  import Numeric.YHSeq.V0111.Type

  reD :: DPN -> Index -> Depth -> Diff
  reD z x n = if x <= 0
    then error "reD: non-positive index"
    else case n `compare` indexN z x of
      LT -> reP z x (n + 1) + reD z (reP z x n) n
      EQ -> indexD z x
      GT -> 0

  reP :: DPN -> Index -> Depth -> Diff
  reP z x n = if x <= 0
    then error "reP: non-positive index"
    else if n > indexN z x
      then 0
      else idx (reverse $ indexP z x) (indexN z x - n)

  decompress :: DPN -> Seq
  decompress z = map (\x -> reD z x 1) (enumFromTo 1 (lengthDPN z))
