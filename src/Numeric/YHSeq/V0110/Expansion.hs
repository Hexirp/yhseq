{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.YHSeq.V0110.Expansion where

  import Numeric.YHSeq.V0110.Type

  badRoot :: DPN -> Index
  badRoot z = last $ indexP z (lengthDPN z)
