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

  type Parent = Integer

  type ParentList = [Parent]

  type Depth = Integer

  type DPNTuple = (Diff, ParentList, Depth)

  type DPN = [DPNTuple]
