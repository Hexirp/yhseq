{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.YHSeq.V0111.Type
  ( module Prelude
  , Seq
  , Index
  , Diff
  , ParentIndex
  , ParentList
  , Depth
  , DPNTuple
  , DPN
  , Class
  , length
  , lengthSeq
  , lengthDPN
  , index
  , index1
  , idx
  , indexSeq
  , indexDPN
  , indexD
  , indexP
  , indexN
  , indexPList
  , slice
  , slice1
  , sliceDPN
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

  type Class = Integer

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
  indexD x n = case indexDPN x n of { (d, _, _) -> d }

  indexP :: DPN -> Index -> ParentList
  indexP x n = case indexDPN x n of { (_, p, _) -> p }

  indexN :: DPN -> Index -> Depth
  indexN x n = case indexDPN x n of { (_, _, n) -> n }

  indexPList :: ParentList -> Index -> ParentIndex
  indexPList = index

  slice :: Integer -> Integer -> [a] -> [a]
  slice a b x = slice' a b x 0
  -- slice a b x =
  --   map fst $
  --     filter (\x -> let n = snd x in a <= n && n <= b) $
  --       zipWith (,) x [0..]

  slice' :: Integer -> Integer -> [a] -> Integer -> [a]
  slice' a b x n = case x of
    []      -> []
    xv : xs -> if a <= n && n <= b
      then xv : slice' a b xs (n + 1)
      else slice' a b xs (n + 1)

  slice1 :: Integer -> Integer -> [a] -> [a]
  slice1 a b x = slice1' a b x 1

  slice1' :: Integer -> Integer -> [a] -> Integer -> [a]
  slice1' a b x n = case x of
    []      -> []
    xv : xs -> if a <= n && n <= b
      then xv : slice1' a b xs (n + 1)
      else slice1' a b xs (n + 1)

  sliceDPN :: Integer -> Integer -> DPN -> DPN
  sliceDPN = slice1
