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

  import Data.Monoid (Sum (..))

  import           Data.IntSet      ( IntSet )
  import qualified Data.IntSet as S
  import           Data.Vector      ( Vector )
  import qualified Data.Vector as V

  -- | 1 から n までの配列に関数を適用する。
  --
  -- 0 が与えられた場合は空の配列を返す。
  genVec :: (Ord i, Integral i) => i -> (i -> a) -> Vector a
  genVec x f = case x `compare` 0 of
    LT -> undefined
    EQ -> V.empty
    GT -> V.map f (V.enumFromTo 1 x)

  -- | 数列、または列。
  newtype Sequence = Sequence { unSequence :: Vector Int }
    deriving stock (Eq, Ord, Show, Read)
    deriving newtype (Semigroup, Monoid)

  -- | 'Sequence' のための添字演算。
  ixSeq :: Sequence -> Int -> Int
  ixSeq s x = unSequence s V.! (x - 1)

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
  -- 添字の集合と言っても、たぶん先祖の集合を表すのにしか使わないと思う。
  newtype IndexSet = IndexSet { unIndexSet :: IntSet }
    deriving stock (Eq, Ord)
    deriving newtype (Show, Read, Semigroup, Monoid)

  -- | 深さ。
  --
  -- YHSS 2.0.1 では、深さは 0 にならない。
  newtype Depth = Depth { unDepth :: Int }
    deriving stock (Eq, Ord, Bounded)
    deriving newtype (Enum, Show, Read, Num, Real, Integral)
    deriving (Semigroup, Monoid) via Sum Int

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
    } deriving stock (Eq, Ord, Show, Read)

  -- | 山から階差を添字で取得する。
  ixMtToDiff :: Mountain -> Index -> Depth -> Difference
  ixMtToDiff z x n = dMt z V.! (unIndex x - 1) V.! (unDepth n - 1)

  -- | 山から親の添字を添字で取得する。
  ixMtToPaet :: Mountain -> Index -> Depth -> Index
  ixMtToPaet z x n = pMt z V.! (unIndex x - 1) V.! (unDepth n - 1)

  -- | 山から先祖の集合を添字で取得する。
  ixMtToAnce :: Mountain -> Index -> Depth -> IndexSet
  ixMtToAnce z x n = aMt z V.! (unIndex x - 1) V.! (unDepth n - 1)

  -- | メモを参照しながら階差を計算する。
  calcDiffOnMtWiM :: Mountain -> Sequence -> Index -> Depth -> Difference
  calcDiffOnMtWiM z s x n = case n `compare` 1 of
    LT -> undefined
    EQ -> Difference (s `ixSeq` unIndex x)
    GT -> case ixMtToPaet z x (n - 1) `compare` 0 of
      LT -> undefined
      EQ -> 0
      GT -> case ixMtToDiff z (ixMtToPaet z x (n - 1)) (n - 1) `compare` 0 of
        LT -> undefined
        EQ -> 0
        GT -> ixMtToDiff z x (n - 1) - ixMtToDiff z (ixMtToPaet z x (n - 1)) (n - 1)

  -- | メモを参照しながら親の添字を計算する。
  calcPaetOnMtWiM :: Mountain -> Index -> Depth -> Index
  calcPaetOnMtWiM z x n = calcPaetOnMtWiM' (x - 1)
   where
    calcPaetOnMtWiM' :: Index -> Index
    calcPaetOnMtWiM' p = case p `compare` 0 of
      LT -> undefined
      EQ -> 0
      GT -> if ixMtToDiff z p n < ixMtToDiff z x n && isAnceAtSh z x n p
        then p
        else calcPaetOnMtWiM' (p - 1)

  -- | メモを参照しながら先祖の集合を計算する。
  calcAnceOnMtWiM :: Mountain -> Index -> Depth -> IndexSet
  calcAnceOnMtWiM z x n = case ixMtToPaet z x n `compare` 0 of
    LT -> undefined
    EQ -> IndexSet (S.singleton (unIndex x))
    GT -> IndexSet (S.insert (unIndex x) (unIndexSet (ixMtToAnce z (ixMtToPaet z x n) n)))

  -- | 一つ浅い深さで先祖であるか。
  --
  -- 名前は at shallow の略である。
  isAnceAtSh :: Mountain -> Index -> Depth -> Index -> Bool
  isAnceAtSh z x n p = case n `compare` 1 of
    LT -> undefined
    EQ -> True
    GT -> unIndex p `S.member` unIndexSet (ixMtToAnce z x (n - 1))

  -- | 山を計算する。
  calcMtFromSeq :: Sequence -> Mountain
  calcMtFromSeq s =
    let
      l = V.length (unSequence s)
      z = Mountain
        { sMt = l
        , dMt = genVec l (\x -> genVec (l + 1) (\n -> calcDiffOnMtWiM z s (Index x) (Depth n)))
        , pMt = genVec l (\x -> genVec (l + 1) (\n -> calcPaetOnMtWiM z (Index x) (Depth n)))
        , aMt = genVec l (\x -> genVec (l + 1) (\n -> calcAnceOnMtWiM z (Index x) (Depth n)))
        }
    in
      z

  -- | クラス。
  newtype Class = Class { unClass :: Int }
    deriving stock (Eq, Ord, Bounded)
    deriving newtype (Enum, Show, Read, Num, Real, Integral)
    deriving (Semigroup, Monoid) via Sum Int

  -- | 共終タイプ。
  --
  -- 順序数に対して定義される数学的な共終数ではない。
  data CofType = IsZero | IsSucc | IsLim Class
    deriving stock (Eq, Ord, Show, Read)

  instance Bounded CofType where
    minBound = IsZero
    maxBound = IsLim maxBound

  -- | 階差が存在する最も大きい深さを、それぞれの添字について計算する。
  --
  -- ここで 0 の深さが現れているが、深さ 1 での階差が 0 でない限り、最終的な計算結果の深さは 0 にならない。
  calcBottom :: Mountain -> Index -> Depth
  calcBottom z x = calcBottom' 0
   where
    calcBottom' :: Depth -> Depth
    calcBottom' n = case ixMtToDiff z x (n + 1) `compare` 0 of
      LT -> undefined
      EQ -> n
      GT -> calcBottom' (n + 1)

  -- | 共終タイプを計算する。
  calcCofType :: Mountain -> CofType
  calcCofType z = case sMt z `compare` 0 of
    LT -> undefined
    EQ -> IsZero
    GT -> case ixMtToPaet z (Index (sMt z)) (calcBottom z (Index (sMt z))) `compare` 0 of
      LT -> undefined
      EQ -> IsSucc
      GT -> IsLim (Class (unDifference (ixMtToDiff z (Index (sMt z)) (calcBottom z (Index (sMt z))))))

  -- DPN 形式。
  --
  -- 条件として dDPN と pDPN と nDPN の長さはそれぞれ sDPN に等しい。
  data DPN = DPN
    { sDPN :: Int
    , dDPN :: Vector Difference
    , pDPN :: Vector (Vector Index)
    , nDPN :: Vector Depth
    } deriving stock (Eq, Ord, Show, Read)

  -- | 展開に関わる最も大きい深さを計算する。
  calcLimitDepth :: Mountain -> Depth
  calcLimitDepth z = calcBottom z (Index (sMt z)) - 1

  -- | 展開に関わる最も大きい深さを、それぞれの添字について計算する。
  calcMaxDepth :: Mountain -> Index -> Depth
  calcMaxDepth z x = calcBottom z x `min` calcLimitDepth z

  -- | DPN 形式での階差の部分を計算する。
  calcDiffOnDpn :: Mountain -> Index -> Difference
  calcDiffOnDpn z x = ixMtToDiff z x (calcMaxDepth z x)

  -- | DPN 形式での親の添字の部分を計算する。
  calcPaetOnDpn :: Mountain -> Index -> Vector Index
  calcPaetOnDpn z x = genVec (calcMaxDepth z x) (\n -> ixMtToPaet z x n)

  -- | DPN 形式での深さの部分を計算する。
  calcNpthOnDpn :: Mountain -> Index -> Depth
  calcNpthOnDpn z x = calcMaxDepth z x

  -- | DPN 形式を計算する。
  calcDpn :: Mountain -> DPN
  calcDpn z = DPN
    { sDPN = sMt z
    , dDPN = genVec (sMt z) (\x -> calcDiffOnDpn z (Index x))
    , pDPN = genVec (sMt z) (\x -> calcPaetOnDpn z (Index x))
    , nDPN = genVec (sMt z) (\x -> calcNpthOnDpn z (Index x))
    }

  -- | 悪部根を計算する。
  calcBadRoot :: Mountain -> Index
  calcBadRoot z = ixMtToPaet z (Index (sMt z)) (calcBottom z (Index (sMt z)) - 1)

  -- | 展開する際の階差の部分を計算する。
  calcDiffAtExp :: Mountain -> Index -> Difference
  calcDiffAtExp z x =
    let
      xz = sMt z
      rz = unIndex (calcBadRoot z)
    in case x >= 1 of
      False -> undefined
      True -> case x >= Index rz of
        False -> calcDiffOnDpn z x
        True ->
          let
            m = (unIndex x - rz) `div` (xz - rz)
            y = Index ((unIndex x - rz) `mod` (xz - rz) + 1)
          in
            calcDiffOnDpn z ((Index rz - 1) + y)

  -- | 展開する際の親の添字の部分を計算する。
  calcPaetAtExp :: Mountain -> Index -> Vector Index
  calcPaetAtExp z x =
    let
      xz = sMt z
      rz = unIndex (calcBadRoot z)
    in case x >= 1 of
      False -> undefined
      True -> case x >= Index rz of
        False -> calcPaetOnDpn z x
        True ->
          let
            m = (unIndex x - rz) `div` (xz - rz)
            y = Index ((unIndex x - rz) `mod` (xz - rz) + 1)
          in
            case m `compare` 0 of
              LT -> undefined
              EQ -> calcPaetOnDpn z ((Index rz - 1) + y)
              GT -> case y `compare` 1 of
                LT -> undefined
                EQ -> genVec (calcNpthOnDpn z ((Index rz - 1) + y)) (\n ->
                  if rz `S.member` unIndexSet (ixMtToAnce z ((Index rz - 1) + y) n)
                    then ixMtToPaet z (Index xz) n + Index ((xz - rz) * (m - 1))
                    else ixMtToPaet z (Index xz) n)
                GT -> genVec (calcNpthOnDpn z ((Index rz - 1) + y)) (\n ->
                  if rz `S.member` unIndexSet (ixMtToAnce z ((Index rz - 1) + y) n)
                    then ixMtToPaet z ((Index rz - 1) + y) n + Index ((xz - rz) * m)
                    else ixMtToPaet z ((Index rz - 1) + y) n)

  -- | 展開する際の深さの部分を計算する。
  calcNpthAtExp :: Mountain -> Index -> Depth
  calcNpthAtExp z x =
    let
      xz = sMt z
      rz = unIndex (calcBadRoot z)
    in case x >= 1 of
      False -> undefined
      True -> case x >= Index rz of
        False -> calcNpthOnDpn z x
        True ->
          let
            m = (unIndex x - rz) `div` (xz - rz)
            y = Index ((unIndex x - rz) `mod` (xz - rz) + 1)
          in
            calcNpthOnDpn z ((Index rz - 1) + y)

  -- | 展開する。
  expand :: Mountain -> Int -> DPN
  expand z n =
    let
      xz = sMt z
      rz = unIndex (calcBadRoot z)
    in
      DPN
        { sDPN = (rz - 1) + (xz - rz) * n
        , dDPN = genVec ((rz - 1) + (xz - rz) * n) (\x -> calcDiffAtExp z (Index x))
        , pDPN = genVec ((rz - 1) + (xz - rz) * n) (\x -> calcPaetAtExp z (Index x))
        , nDPN = genVec ((rz - 1) + (xz - rz) * n) (\x -> calcNpthAtExp z (Index x))
        }

  -- | メモを参照しながら DPN 形式から山の親の添字の部分を計算する。
  calcPaetOnMtFromDpnWiM :: Mountain -> DPN -> Index -> Depth -> Index
  calcPaetOnMtFromDpnWiM zm zd x n = case n >= 1 of
    False -> undefined
    True -> undefined

  -- | DPN 形式から山を計算する。
  calcMtFromDpn :: DPN -> Mountain
  calcMtFromDpn zd =
    let
      l = sDPN zd
      zm = Mountain
        { sMt = l
        , dMt = genVec l (\x -> genVec (l + 1) (\n -> undefined (Index x) (Depth n)))
        , pMt = genVec l (\x -> genVec (l + 1) (\n -> undefined (Index x) (Depth n)))
        , aMt = genVec l (\x -> genVec (l + 1) (\n -> undefined (Index x) (Depth n)))
        }
    in
      zm
