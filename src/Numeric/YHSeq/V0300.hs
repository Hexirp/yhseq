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
  fromSeqToMt (Sequence s) = let len_s = V.length s in Mountain
    { difference = generate len_s (\n -> generate len_s (\x -> diffs n x s))
    , parent     = generate len_s (\n -> generate len_s (\x -> paets n x s))
    , ancestor   = generate len_s (\n -> generate len_s (\x -> ances n x s))
    }
