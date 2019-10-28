{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.YHSeq.V0111.Compression
  ( mtD
  , mtP
  , searchParent
  , searchParentAnc
  , anc
  , nonEmptyDepth
  , cl
  , compressionDepth
  , cM
  , cU
  , btm
  , cD
  , cP
  , cN
  , seqClass
  , compress
  ) where

  import Numeric.YHSeq.V0111.Type

  mtD :: Seq -> Index -> Depth -> Diff
  mtD s x n = if x <= 0
    then error "mtD: non-positive index"
    else if n <= 0
      then error "mtD: non-positive depth"
      else mtD' s x n

  mtD' :: Seq -> Index -> Depth -> Diff
  mtD' s x n = case n `compare` 1 of
    LT -> error "mtD: impossible case"
    EQ -> indexSeq s x
    GT -> case mtP s x (n - 1) `compare` 0 of
      LT -> error "mtD: irregular value of mtP"
      EQ -> 0
      GT -> mtD s x (n - 1) - mtD s (mtP s x (n - 1)) (n - 1)

  mtP :: Seq -> Index -> Depth -> ParentIndex
  mtP s x n = if x <= 0
    then error "mtP: non-positive index"
    else if n <= 0
      then error "mtP: non-positive depth"
      else mtP' s x n (x - 1)

  mtP' :: Seq -> Index -> Depth -> ParentIndex -> ParentIndex
  mtP' s x n p = case p `compare` 0 of
    LT -> error "mtP: impossible case"
    EQ -> 0
    GT -> if mtD s p n < mtD s x n && isAnc s x n p
      then p
      else mtP' s x n (p - 1)

  isAnc :: Seq -> Index -> Depth -> ParentIndex -> Bool
  isAnc s x n p = if x <= 0
    then error "isAnc: non-positive index"
    else case n `compare` 0 of
      LT -> error "isAnc: non-positive depth"
      EQ -> True
      GT -> p `elem` anc s x (n - 1)

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


  btm :: Seq -> Index -> Depth
  btm s x = if x <= 0
    then error "btm: non-positive index"
    else btm' s x 1

  btm' :: Seq -> Index -> Depth -> Depth
  btm' s x n = if not (mtD s x (n + 1) > 0)
    then n
    else btm' s x (n + 1)

  nonEmptyDepth :: Seq -> Depth
  nonEmptyDepth s = btm s 1

  cl :: Seq -> Class
  cl s = mtD s (lengthSeq s) (nonEmptyDepth s)

  compressionDepth :: Seq -> Depth
  compressionDepth s = case cl s `compare` 1 of
    LT -> error "compressionDepth: irregular value of cl"
    EQ -> nonEmptyDepth s - 1
    GT -> nonEmptyDepth s

  cN :: Seq -> Index -> Depth
  cN s x = if x <= 0
    then error "cN: non-positive index"
    else compressionDepth s `min` btm s x

  cU :: Seq -> Index -> Depth
  cU s x = if x <= 0
    then error "cU: non-positive index"
    else cU' s x 1

  cU' :: Seq -> Index -> Depth -> Depth
  cU' s x n = if not (mtP s x 1 == mtP s x (n + 1) && n + 1 <= btm s x)
    then n
    else cU' s x (n + 1)

  cM :: Seq -> Index -> Depth
  cM s x = if x <= 0
    then error "cM: non-positive index"
    else cN s x `min` cU s x

  cD :: Seq -> Index -> Diff
  cD s x = if x <= 0
    then error "cD: non-positive index"
    else mtD s x (cN s x)

  cP :: Seq -> Index -> ParentList
  cP s x = if x <= 0
    then error "cP: non-positive index"
    else map (\p -> mtP s x p) $ enumFromTo (cM s x) (cN s x)


  seqClass :: Seq -> Class
  seqClass = cl

  compress :: Seq -> DPN
  compress s = map (\x -> (cD s x, cP s x, cN s x)) $ enumFromTo 1 (lengthSeq s)
