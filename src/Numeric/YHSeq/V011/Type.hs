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

  index :: [a] -> Integer -> a
  index x n = if n < 0
    then error "index: negative index"
    else case x of
      [] -> error "index: index too large"
      xv : xs -> if n == 0
        then xv
        else index xs (n - 1)
