-- Y数列 Hexirp 版 3.0 の定義。
--
-- * https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html
-- * https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-IntSet.html
--
-- 以上の二つのライブラリは重要である。
module Numeric.YHSeq.V0300 where

  import Prelude

  import           Data.IntSet      ( IntSet )
  import qualified Data.IntSet as S
  import           Data.Vector      ( Vector )
  import qualified Data.Vector as V

  -- 数列
  newtype Sequence = Sequence { unSeq :: Vector Int } deriving (Eq, Ord, Show)

  -- 山
  data Mountain = Mountain
    { -- 階差
      diff :: Vector (Vector Int)
    , -- 親の情報
      paet :: Vector (Vector Int)
    , -- 先祖の情報
      ance :: Vector (Vector IntSet)
    } deriving (Eq, Ord, Show)

  -- 山から階差を得る
  diffz :: Mountain -> Int -> Int -> Int
  diffz z x n = (diff z V.! (x - 1)) V.! (n - 1)

  -- 山から親の情報を得る
  paetz :: Mountain -> Int -> Int -> Int
  paetz z x n = (paet z V.! (x - 1)) V.! (n - 1)

  -- 山から先祖の情報を得る
  ancez :: Mountain -> Int -> Int -> IntSet
  ancez z x n = (ance z V.! (x - 1)) V.! (n - 1)

  -- 数列から山を構築する
  fromSeqToMt :: Sequence -> Mountain
  fromSeqToMt s =
    let
      diffs :: Mountain -> Sequence -> Int -> Int -> Int
      diffs z s x n = case n `compare` 1 of
        LT -> undefined
        EQ -> unSeq s V.! (x - 1)
        GT -> case paetz z x (n - 1) `compare` 0 of
          LT -> undefined
          EQ -> 0
          GT -> case diffz z (paetz z x (n - 1)) (n - 1) `compare` 0 of
            LT -> undefined
            EQ -> 0
            GT -> diffz z x (n - 1) - diffz z (paetz z x (n - 1)) (n - 1)
      paets :: Mountain -> Int -> Int -> Int
      paets z x n = paets' z x n (x - 1)
      paets' :: Mountain -> Int -> Int -> Int -> Int
      paets' z x n p = case p `compare` 0 of
        LT -> undefined
        EQ -> 0
        GT -> if diffz z p n < diffz z x n && is_ancez z x n p
          then p
          else paets' z x n (p - 1)
      is_ancez :: Mountain -> Int -> Int -> Int -> Bool
      is_ancez z x n p = case n `compare` 1 of
        LT -> undefined
        EQ -> True
        GT -> S.member p (ancez z x (n - 1))
      ances :: Mountain -> Int -> Int -> IntSet
      ances z x n = case x `compare` 0 of
        LT -> undefined
        EQ -> S.empty
        GT -> S.insert x (ancez z (paetz z x n) n)
    in
      let
        len_s = V.length (unSeq s)
        gen_s = \f -> V.map f (V.enumFromTo 1 len_s)
        z = Mountain
          { diff = gen_s (\x -> gen_s (\n -> diffs z s x n))
          , paet = gen_s (\x -> gen_s (\n -> paets z x n))
          , ance = gen_s (\x -> gen_s (\n -> ances z x n))
          }
      in
        z
