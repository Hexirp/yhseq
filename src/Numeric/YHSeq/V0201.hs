{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

-- | YH数列システム 2.0.1 の定義。
--
-- ここでは "Data.IntSet" と "Data.Vector" を活用している。
--
-- 英語では YH sequence system 2.0.1 と呼ぶことにする。短く呼ぶときは YHSS 2.0.1 と呼ぶことにする。
module Numeric.YHSeq.V0201 where

  import Prelude

  import Data.Monoid (Sum)

  import           Data.IntSet      ( IntSet )
  import qualified Data.IntSet as S
  import           Data.Vector      ( Vector )
  import qualified Data.Vector as V

  -- | 数列、または列。
  newtype Sequence = Sequence { unSequence :: Vector Int }
    deriving stock (Eq, Ord, Show, Read)
    deriving newtype (Semigroup, Monoid)

  -- | 'Sequence' のための添字演算。
  ixS :: Sequence -> Int -> Int
  ixS s x = unSeq s V.! (x - 1)

  -- | 階差。
  --
  -- 数列の階差を取った列を階差列と呼ぶことにする。たとえば (1,2,4,8,16,...) の階差列は (0,1,2,4,8,...) だ。
  newtype Difference = Difference { unDifference :: Int }
    deriving stock (Eq, Ord, Bounded)
    deriving newtype (Enum, Show, Read, Num, Real, Integral)
    deriving (Semigroup, Monoid) via Sum Int

  -- | 添字。
  --
  -- 添字といっても 'Sequence' の添字ではなく親を参照したりするときに使う添字だ。 'ixS' を見てもらえば分かるように、 'Sequence' の添字は普通の 'Int' だ。
  newtype Index = Index { unIndex :: Int }
    deriving stock (Eq, Ord, Bounded)
    deriving newtype (Enum, Show, Read, Num, Real, Integral)
    deriving (Semigroup, Monoid) via Sum Int

  -- | 添字の集合。
  --
  -- 添字の集合と言っても、たぶん先祖の集合を表すのしか使わないと思う。
  newtype IndexSet = IndexSet { unIndexSet :: IntSet }
    deriving stock (Eq, Ord)
    deriving newtype (Show, Read, Semigroup, Monoid)

  -- | 山。
  --
  -- 条件として dMt と pMt と aMt の長さはそれぞれ sMt に等しい。
  data Mountain = Mountain
    { -- | 山のサイズ。
      sMt :: Int
    , -- | 階差。
      dMt :: Vector (Vector Difference)
    , -- | 親の添字。
      pMt :: Vector (Vector Index)
    , -- | 先祖の集合。
      aMt :: Vector (Vector IndexSet)
    } deriving (Eq, Ord, Show, Read)
