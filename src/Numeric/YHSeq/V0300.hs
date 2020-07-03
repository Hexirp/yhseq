module Numeric.YHSeq.V0300 where

  import Prelude

  import           Data.IntSet      ( IntSet )
  import qualified Data.IntSet as S
  import           Data.Vector      ( Vector )
  import qualified Data.Vector as V

  -- 数列
  newtype Sequence = Sequence { unSequence :: Vector Int }

  -- 山
  data Mountain = Mountain
    { difference :: Vector (Vector Int)
    , parent     :: Vector (Vector Int)
    , ancestor   :: Vector (Vector IntSet)
    }

  fromSeqToMt :: Sequence -> Mountain
  fromSeqToMt (Sequence s) = let len_s = V.length s in
    let z = Mountain
      { difference = generate len_s (\n -> generate len_s (\x -> diffs z s n x))
      , parent     = generate len_s (\n -> generate len_s (\x -> paets z s n x))
      , ancestor   = generate len_s (\n -> generate len_s (\x -> ances z s n x))
      }

  diffs :: Mountain -> Sequence -> Int -> Int -> Int
  diffs m s n x = undefined
