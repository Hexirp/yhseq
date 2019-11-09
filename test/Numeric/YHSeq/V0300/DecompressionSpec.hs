module Numeric.YHSeq.V0300.DecompressionSpec (spec) where

  import Prelude hiding (length)

  import Test.Hspec

  import Numeric.YHSeq.V0300.Type
  import Numeric.YHSeq.V0300.Compression (compress)
  import Numeric.YHSeq.V0300.Decompression

  spec :: Spec
  spec = do

    describe "decompress" $ do

      it "is a inverse function of `compress` at (1,2,4,5,4)" $ do
        decompress (compress [1,2,4,5,4]) `shouldBe` [1,2,4,5,4]

      it "is a inverse function of `compress` at (1,2,4,6)" $ do
        decompress (compress [1,2,4,6]) `shouldBe` [1,2,4,6]

      it "is a inverse function of `compress` at (1,2,4,8)" $ do
        decompress (compress [1,2,4,8]) `shouldBe` [1,2,4,8]

      it "is a inverse function of `compress` at (1,2,4,8,10,8)" $ do
        decompress (compress [1,2,4,8,10,8]) `shouldBe` [1,2,4,8,10,8]

      it "is a inverse function of `compress` at (1,3)" $ do
        decompress (compress [1,3]) `shouldBe` [1,3]

      it "is a inverse function of `compress` at (1,3,3)" $ do
        decompress (compress [1,3,3]) `shouldBe` [1,3,3]

      it "is a inverse function of `compress` at (1,3,5)" $ do
        decompress (compress [1,3,5]) `shouldBe` [1,3,5]
