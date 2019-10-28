{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.YHSeq.V0111.Expansion
  ( badRoot
  , goodPoot
  , badPart
  , cuttedPart
  , pnt
  , anc
  , badRootL
  , delta
  , amt
  , bas
  , rising
  , ris
  , newD
  , newP
  , newN
  , copiedBadPart
  , expand
  ) where

  import Numeric.YHSeq.V0111.Type

  badRoot :: DPN -> Index
  badRoot z = last $ indexP z (lengthDPN z)

  goodPart :: DPN -> DPN
  goodPart z = sliceDPN 1 (badRoot z - 1) z

  badPart :: DPN -> DPN
  badPart z = sliceDPN (badRoot z) (lengthDPN z - 1) z

  cuttedPart :: DPN -> DPN
  cuttedPart z = sliceDPN (lengthDPN z) (lengthDPN z) z


  pnt :: DPN -> Index -> Depth -> ParentIndex
  pnt z x n = if x <= 0
    then error "pnt: non-positive index"
    else if n > indexN z x
      then 0
      else idx (reverse $ indexP z x) (indexN z x - n)

  anc :: DPN -> Index -> Depth -> [ParentIndex]
  anc z x n = if x <= 0
    then error "anc: non-positive index"
    else if n <= 0
      then error "anc: non-positive depth"
      else anc' z x n

  anc' :: DPN -> Index -> Depth -> [ParentIndex]
  anc' z x n = case x `compare` 0 of
    LT -> error "anc: irregular value of pnt"
    EQ -> []
    GT -> x : anc' z (pnt z x n) n


  -- bad root (length)
  badRootL :: DPN -> Index
  badRootL z = badRoot z

  delta :: DPN -> Integer
  delta z = lengthDPN z - badRootL z

  -- ascension matrix
  amt :: DPN -> Index -> Bool
  amt z y = badRootL z `elem` anc z (badRootL z - 1 + y) 1

  bas :: DPN -> Index -> ParentList
  bas z y = if y == 1
    then indexP z (lengthDPN z)
    else indexP z (badRootL z - 1 + y)

  rising :: DPN -> Integer -> Index -> ParentIndex -> ParentIndex
  rising z m y p = if amt z y
    then p + m * delta z
    else p

  ris :: DPN -> Integer -> Index -> ParentList -> ParentList
  ris z m y p = map (\q -> rising z m y q) p


  newD :: DPN -> Integer -> Index -> Diff
  newD z m y = indexD z (badRootL z - 1 + y)

  newP :: DPN -> Integer -> Index -> ParentList
  newP z m y = if y == 1
    then ris z (m - 1) y (bas z y)
    else ris z m y (bas z y)

  newN :: DPN -> Integer -> Index -> Depth
  newN z m y = indexN z (badRootL z - 1 + y)

  copiedBadPart :: DPN -> Integer -> DPN
  copiedBadPart z m = if m == 0
    then badPart z
    else map (\y -> (newD z m y, newP z m y, newN z m y)) $
      enumFromTo 1 (delta z)


  expand :: DPN -> Class -> Integer -> DPN
  expand z c n = case c `compare` 1 of
    LT -> error "expand: non-positive class"
    EQ -> goodPart z ++ concat (map (\m -> copiedBadPart z m) $ enumFromTo 0 n)
    GT -> error "expand: is undefined at 0.1.1.0 when class is greater than 1"
