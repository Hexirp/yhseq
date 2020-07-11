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

  -- * 数列と山

  -- 数列
  newtype Sequence = Sequence { unSeq :: Vector Int } deriving (Eq, Ord, Show)

  -- 山、条件として diff と paet と ance の長さは size に等しい
  data Mountain = Mountain
    { -- サイズ
      size :: Int
    , -- 階差
      diff :: Vector (Vector Int)
    , -- 親の情報
      paet :: Vector (Vector Int)
    , -- 先祖の情報
      ance :: Vector (Vector IntSet)
    } deriving (Eq, Ord, Show)

  -- 山から階差を得る
  diffz :: Mountain -> Int -> Int -> Int
  diffz z x n = diff z V.! (x - 1) V.! (n - 1)

  -- 山から親の情報を得る
  paetz :: Mountain -> Int -> Int -> Int
  paetz z x n = paet z V.! (x - 1) V.! (n - 1)

  -- 山から先祖の情報を得る
  ancez :: Mountain -> Int -> Int -> IntSet
  ancez z x n = ance z V.! (x - 1) V.! (n - 1)

  -- 数列から山を構築する
  fromSeqToMt :: Sequence -> Mountain
  fromSeqToMt s =
    let
      len_s = V.length (unSeq s)
      gen x = \f -> V.map f (V.enumFromTo 1 x)
      z = Mountain
        { size = len_s
        , diff = gen len_s (\x -> gen (len_s + 1) (\n -> diffs z s x n))
        , paet = gen len_s (\x -> gen (len_s + 1) (\n -> paets z x n))
        , ance = gen len_s (\x -> gen (len_s + 1) (\n -> ances z x n))
        }
    in
      z
   where
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
     where
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
    ances z x n = case paetz z x n `compare` 0 of
      LT -> undefined
      EQ -> S.singleton x
      GT -> S.insert x (ancez z (paetz z x n) n)

  -- * クラス分け

  data Class = IsZero | IsSucc | IsLim Int deriving (Eq, Ord, Show)

  -- 0 ではない階差が存在する最も大きい深さ
  mtBottom :: Mountain -> Int -> Int
  mtBottom z x = mtBottom' z x 1
   where
    mtBottom' :: Mountain -> Int -> Int -> Int
    mtBottom' z x n = case diffz z x (n + 1) `compare` 0 of
      LT -> undefined
      EQ -> n
      GT -> mtBottom' z x (n + 1)

  -- 山のクラス
  mtClass :: Mountain -> Class
  mtClass z = case size z `compare` 0 of
    LT -> undefined
    EQ -> IsZero
    GT -> case paetz z (size z) (mtBottom z (size z)) `compare` 0 of
      LT -> undefined
      EQ -> IsSucc
      GT -> IsLim (diffz z (size z) (mtBottom z (size z)))

  -- * クラスが IsLim 1 である山の展開

  -- 非零最深度、上限の深さ、ここまでの深さの値だけを展開に使用する
  mtMaxDepth_L1 :: Mountain -> Int
  mtMaxDepth_L1 z = mtBottom z (size z) - 1

  -- 真の悪部根、良部と悪部を決定する
  mtTrueBadRoot :: Mountain -> Int
  mtTrueBadRoot z = paetz z (size z) (mtMaxDepth_L1 z)

  -- 良部の長さ
  mtGoodPartLen_L1 :: Mountain -> Int
  mtGoodPartLen_L1 z = mtTrueBadRoot z - 1

  -- 悪部の長さ
  mtBadPartLen_L1 :: Mountain -> Int
  mtBadPartLen_L1 z = size z - mtTrueBadRoot z - 1

  -- 展開後の山のサイズ（数列の長さ）
  mtNewSize_L1 :: Mountain -> Int -> Int
  mtNewSize_L1 z m = mtGoodPartLen_L1 z + mtBadPartLen_L1 z * (1 + m)

  -- DPN 形式
  data DPN = DPN
    { sDPN :: Int
    , dDPN :: Vector Int
    , pDPN :: Vector (Vector Int)
    , nDPN :: Vector Int
    }

  -- 山から DPN 形式へ
  fromMtToDPN :: Mountain -> Vector (Int, Vector Int, Int)
  fromMtToDPN = undefined

  -- クラスが IsLim 1 である山を展開する
  expand_L1 :: Mountain -> Int -> Mountain
  expand_L1 z =
    let
      paets :: Mountain -> Int -> Int -> Int
      paets z x n = case x `compare` 1 of
        LT -> undefined
        _  -> case x `compare` mtTrueBadRoot z of
          LT -> paetz z x n
          _  -> let f a b x = ((x - a) `mod` b) + a in
            paetz z (f (mtGoodPartLen_L1 z) (mtBadPartLen_L1 z) x) n
    in
      undefined

  -- * クラスが IsLim (n + 1) である山の展開

  -- 上限の深さ、ここまでの深さの値だけを展開に使用する
  mtMaxDepth_Ln :: Mountain -> Int
  mtMaxDepth_Ln z = mtBottom z (size z)

  -- 偽の悪部根、対角列を決定する
  mtFalseBadRoot :: Mountain -> Int
  mtFalseBadRoot z = paetz z (size z) (mtMaxDepth_Ln z)

  -- 上限の深さを超えない最大の深さ
  mtBottom_Ln :: Mountain -> Int -> Int
  mtBottom_Ln z x = mtBottom z x `min` mtMaxDepth_Ln z

  -- 偽の悪部根の深さ、対角列を決定する
  mtFaBaRoDepth :: Mountain -> Int
  mtFaBaRoDepth z = mtBottom_Ln z (mtFalseBadRoot z)

  -- 偽の悪部根の先祖、対角列を決定する
  mtFaBaRoAnce :: Mountain -> IntSet
  mtFaBaRoAnce z = mtFaBaRoAnce' (mtFalseBadRoot z) (mtFaBaRoDepth z)
   where
    mtFaBaRoAnce' x n = case n `compare` 1 of
      LT -> undefined
      EQ -> case paetz z x n `compare` 0 of
        LT -> undefined
        EQ -> S.singleton x
        GT -> S.insert x (mtFaBaRoAnce' (paetz z x n) n)
      GT -> case paetz z x n `compare` 0 of
        LT -> undefined
        EQ -> S.insert x (mtFaBaRoAnce' (paetz z x n) (n - 1))
        GT -> S.insert x (mtFaBaRoAnce' (paetz z x n) n)

  -- 対角列に含めるか
  mtDiagBool :: Mountain -> Int -> Bool
  mtDiagBool z x = False
    || S.member x (mtFaBaRoAnce z)
    || S.member (mtFalseBadRoot z) (ancez z x (mtFaBaRoDepth z))

  -- 対角列
  mtDiagonal :: Mountain -> Sequence
  mtDiagonal z = Sequence (V.filter (mtDiagBool z) (V.enumFromTo 1 (size z)))

  -- クラスが IsLim (n + 1) である山を展開する
  expand_Ln :: Mountain -> Mountain
  expand_Ln z = undefined

  -- * テスト

  -- 通常のリストから山を構築する
  fromListToMt :: [Int] -> Mountain
  fromListToMt x = fromSeqToMt (Sequence (V.fromList x))

  -- テスト
  test :: IO ()
  test = do
    print "fromSeqToMt $ Sequence $ V.fromList $ [1,2,4,8,10,8]"
    print (fromSeqToMt $ Sequence $ V.fromList $ [1,2,4,8,10,8])
    print "mtClass $ fromListToMt []"
    print (mtClass $ fromListToMt [])
    print "mtClass $ fromListToMt [1,2,1]"
    print (mtClass $ fromListToMt [1,2,1])
    print "mtClass $ fromListToMt [1,2,4,8,10,8]"
    print (mtClass $ fromListToMt [1,2,4,8,10,8])
    print "mtClass $ fromListToMt [1,3]"
    print (mtClass $ fromListToMt [1,3])
    print "mtClass $ fromListToMt [1,3,4,2,5,6,5]"
    print (mtClass $ fromListToMt [1,3,4,2,5,6,5])
    print "mtDiagonal $ fromListToMt [1,3]"
    print (mtDiagonal $ fromListToMt [1,3])
    print "mtDiagonal $ fromListToMt [1,3,3]"
    print (mtDiagonal $ fromListToMt [1,3,3])
    print "mtDiagonal $ fromListToMt [1,3,5]"
    print (mtDiagonal $ fromListToMt [1,3,5])
    print "mtDiagonal $ fromListToMt [1,3,4,2,5,6,5]"
    print (mtDiagonal $ fromListToMt [1,3,4,2,5,6,5])
