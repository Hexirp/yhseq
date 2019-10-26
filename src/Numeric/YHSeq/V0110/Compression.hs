{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.YHSeq.V0110.Compression
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

  {-

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
    else case n `compare` 0
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

  -}


  nonEmptyDepth :: Seq -> Depth
  nonEmptyDepth s = nonEmptyDepth' s 1

  nonEmptyDepth' :: Seq -> Depth -> Depth
  nonEmptyDepth' s n = if not (mtD s (lengthSeq s) (n + 1) > 0)
    then n
    else nonEmptyDepth' s (n + 1)

  cl :: Seq -> Integer
  cl s = mtD s (lengthSeq s) (nonEmptyDepth s)

  compressionDepth :: Seq -> Depth
  compressionDepth s = case cl s `compare` 1 of
    LT -> error "compressionDepth: irregular value of cl"
    EQ -> nonEmptyDepth s - 1
    GT -> nonEmptyDepth s


  btm :: Seq -> Index -> Depth
  btm s x = if x <= 0
    then error "btm: non-positive index"
    else btm' s x 1

  btm' :: Seq -> Index -> Depth -> Depth
  btm' s x n = if not (mtD s x (n + 1) > 0)
    then n
    else btm' s x (n + 1)

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


  seqClass :: Seq -> Integer
  seqClass = cl

  compress :: Seq -> DPN
  compress s = map (\x -> (cD s x, cP s x, cN s x)) $ enumFromTo 1 (lengthSeq s)
