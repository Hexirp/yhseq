module Numeric.YHSeq.V0300 where

  import Prelude

  import           Data.IntSet      ( IntSet )
  import qualified Data.IntSet as S
  import           Data.Vector      ( Vector )
  import qualified Data.Vector as V

  -- 数列
  newtype Sequence = Sequence { unSeq :: Vector Int }

  -- 山
  data Mountain = Mountain
    { -- 階差
      diff :: Vector (Vector Int)
    , -- 親の情報
      paet :: Vector (Vector Int)
    , -- 先祖の情報
      ance :: Vector (Vector IntSet)
    }

  -- 山から階差を得る
  diffz :: Mountain -> Int -> Int -> Int
  diffz z x n = (diff z ! x) ! n

  -- 山から親の情報を得る
  paetz :: Mountain -> Int -> Int -> Int
  paetz z x n = (paet z ! x) ! n

  -- 山から先祖の情報を得る
  ancez :: Mountain -> Int -> Int -> IntSet
  ancez z x n = (ance z ! x) ! n

  -- 数列から山を構築する
  fromSeqToMt :: Sequence -> Mountain
  fromSeqToMt s =
    let
      diffs :: Mountain -> Sequence -> Int -> Int -> Int
      diffs z s x n = case n `compare` 1 of
        LT -> undefined
        EQ -> unSeq s V.! x
        GT -> case paetz z x (n - 1) `compare` 0 of
          LT -> undefined
          EQ -> 0
          GT -> case diffz z (paetz z x (n - 1)) (n - 1) of
            LT -> undefined
            EQ -> 0
            GT -> diffz z x (n - 1) - diffz z (paetz z x (n - 1)) (n - 1)
      paets :: Mountain -> Int -> Int -> Int
      paets z x n = paets' z x n (x - 1)
      paets' :: Mountain -> Int -> Int -> Int -> Int
      paets' z x n p = case p `compare` 0 of
        LT -> undefined
        EQ -> 0
        GT -> if diffz z p n < diffz z x n && in_ances z x n p
          then p
          else paets' z x n (p - 1)
      ances :: Mountain -> Int -> Int -> IntSet
      ances z x n = case x `compare` 0 of
        LT -> undefined
        EQ -> S.empty
        GT -> S.insert x (ances z (paets z x n) n)
      in_ances :: Mountain -> Int -> Int -> Bool
      in_ances z x n p = case n `compare` 1 of
        LT -> undefined
        EQ -> True
        GT -> S.member p (ances s x (n - 1))
    in
      let
        len_s = V.length s
        z = Mountain
          { diff = V.generate len_s (\x -> V.generate len_s (\n -> diffs z s x n))
          , paet = V.generate len_s (\x -> V.generate len_s (\n -> paets z x n))
          , ance = V.generate len_s (\x -> V.generate len_s (\n -> ances z x n))
          }
      in
        z
