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
