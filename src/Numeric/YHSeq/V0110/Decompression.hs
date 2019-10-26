{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.YHSeq.V0110.Decompression where

  import Numeric.YHSeq.V0110.Type

  reD :: DPN -> Index -> Depth -> Diff
  reD z x n = case n `compare` indexN z x of
    LT -> 0
    EQ -> indexD z x
    GT -> reP z x (n + 1) + reD z (reP z x n) n

  reP :: DPN -> Index -> Depth -> Diff
  reP z x n = if n > indexN z x
    then 0
    else idx (reverse $ indexD z x) (indexN z x - n)

  decompress :: DPN -> Seq
  decompress z = map (\x -> reD z x 1) (enumFromTo 1 (lengthDPN z))
