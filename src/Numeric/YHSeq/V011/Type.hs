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

  index1 :: [a] -> Integer -> a
  index1 x n = if n < 1
    then error "index1: not positive index"
    else case x of
      [] -> error "index1: index too large"
      xv : xs -> if n == 1
        then xv
        else index xs (n - 1)

  indexSeq :: Seq -> Integer -> Integer
  indexSeq = index1

  indexDPN :: DPN -> Integer -> DPNTuple
  indexDPN = index1

  indexPList :: ParentList -> Integer -> Parent
  indexPList = index

  idx :: [a] -> Integer -> a
  idx x n = if n < 0
    then error "idx: negative index"
    else case x of
      [] -> error "idx: empty list"
      xv : xs -> idx' xv xs n

  idx' :: a -> [a] -> Integer -> a
  idx' a x n = if n == 0
    then a
    else case x of
      [] -> a
      xv : xs -> idx' xv xs (n - 1)
