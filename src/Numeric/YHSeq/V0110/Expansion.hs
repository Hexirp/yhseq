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
