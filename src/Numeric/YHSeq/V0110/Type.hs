{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.YHSeq.V0110.Type
  ( module Prelude
  , Seq
  , Index
  , Diff
  , ParentIndex
  , ParentList
  , Depth
  , DPNTuple
  , DPN
  , length
  , lengthSeq
  , lengthDPN
  , index
  , index1
  , idx
  , indexSeq
  , indexDPN
  , indexPList
  ) where

  import Prelude hiding (length)

  type Seq = [Integer]

  type Index = Integer

  type Diff = Integer

  type ParentIndex = Index

  type ParentList = [ParentIndex]

  type Depth = Integer

  type DPNTuple = (Diff, ParentList, Depth)

  type DPN = [DPNTuple]

  length :: [a] -> Integer
  length []      = 0
  length (_ : s) = length s + 1

  lengthSeq :: Seq -> Integer
  lengthSeq = length

  lengthDPN :: DPN -> Integer
  lengthDPN = length

  index :: [a] -> Integer -> a
  index x n = if n < 0
    then error "index: negative index"
    else index' x n

  index' :: [a] -> Integer -> a
  index' x n = case x of
    []      -> error "index: index too large"
    xv : xs -> if n == 0
      then xv
      else index' xs (n - 1)

  index1 :: [a] -> Integer -> a
  index1 x n = if n < 1
    then error "index1: non-positive index"
    else index1' x n

  index1' :: [a] -> Integer -> a
  index1' x n = case x of
    []      -> error "index1: index too large"
    xv : xs -> if n == 1
      then xv
      else index1' xs (n - 1)

  idx :: [a] -> Integer -> a
  idx x n = if n < 0
    then error "idx: negative index"
    else case x of
      []      -> error "idx: empty list"
      xv : xs -> idx' xv xs n

  idx' :: a -> [a] -> Integer -> a
  idx' a x n = if n == 0
    then a
    else case x of
      []      -> a
      xv : xs -> idx' xv xs (n - 1)

  indexSeq :: Seq -> Index -> Integer
  indexSeq = index1

  indexDPN :: DPN -> Index -> DPNTuple
  indexDPN = index1

  indexD :: DPN -> Index -> Diff
  indexD = \x -> case indexDPN x of { (d, _, _) -> d }

  indexP :: DPN -> Index -> ParentList
  indexP = \x -> case indexDPN x of { (_, p, _) -> p }

  indexN :: DPN -> Index -> Depth
  indexN = \x -> case indexDPN x of { (_, _, n) -> n }

  indexPList :: ParentList -> Index -> ParentIndex
  indexPList = index
