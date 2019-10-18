module Numeric.YHSeq.V011.Type
  ( last
  , map
  , reverse
  , concat
  ) where

  import Prelude

  import Data.LIst

  type Seq = [Integer]

  type Diff = Integer

  type Parent = [Integer]

  type Depth = Integer

  type DPNTuple = (Diff, Parent, Depth)

  type DPN = [DPNTuple]
