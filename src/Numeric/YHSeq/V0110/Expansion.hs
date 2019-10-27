{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.YHSeq.V0110.Expansion where

  import Numeric.YHSeq.V0110.Type

  badRoot :: DPN -> Index
  badRoot z = last $ indexP z (lengthDPN z)

  goodPart :: DPN -> DPN
  goodPart z = sliceDPN 1 (badRoot z - 1) z

  badPart :: DPN -> DPN
  badPart z = sliceDPN (badRoot z) (lengthDPN z - 1)

  cuttedPart :: DPN -> DPN
  cuttedPart z = sliceDPN (lengthDPN z) (lengthDPN z)


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
  amt z y = badRootL `elem` anc z (badRootL - 1 + y) 1

  bas :: DPN -> Index -> ParentList
  bas z y = if y == 1
    then indexP z (lengthDPN z)
    else indexP z (badRootL z - 1 + y)

  rising :: DPN -> Integer -> Index -> ParentList -> ParentIndex
  rising z m y p = p + m * delta z * boolToInteger (amt z y)

  ris :: DPN -> Integer -> Index -> ParentList -> ParentList
  ris z m y p = map (\q -> rising z m y q) p
