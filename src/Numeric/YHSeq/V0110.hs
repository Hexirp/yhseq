{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.YHSeq.V0110 where

  import Numeric.YHSeq.V0110.Type
  import Numeric.YHSeq.V0110.Compression   (seqClass, compress)
  import Numeric.YHSeq.V0110.Expansion     (expand)
  import Numeric.YHSeq.V0110.Decompression (decompress)

  data Cofinal = IsZero | IsSucc | IsLimit

  isZero :: Seq -> Bool
  isZero s = s == []

  isSucc :: Seq -> Bool
  isSucc s = indexSeq s (lengthSeq s) == 1

  isLimit :: Seq -> Bool
  isLimit s = not (isZero s) && not (isSucc s)

  -- sequence cofinality
  scof :: Seq -> Cofinal
  scof s = case s of
    []    -> IsZero
    _ : _ -> if last s == 1
      then IsSucc
      else IsLimit

  -- fundamental sequence
  fseq :: Seq -> Integer -> Seq
  fseq s n = if isLimit s
    then decompress (expand (compress s) (seqClass s) n)
    else error "fseq: it is not limit sequence"

  -- predecessor sequence
  pseq :: Seq -> Seq
  pseq s = if isSucc s
    then take (lengthSeq s - 1) s
    else error "pseq: it is not successor sequence"

  -- | It's a very very large function.
  yhseq :: Seq -> Integer -> Integer
  yhseq s n = case scof s of
    IsZero  -> n
    IsSucc  -> yhseq (pseq s) (n + 1)
    IsLimit -> yhseq (fseq s n) (n + 1)
