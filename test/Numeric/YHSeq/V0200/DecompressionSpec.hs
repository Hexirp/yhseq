module Numeric.YHSeq.V0200.DecompressionSpec (spec) where

  import Prelude hiding (length)

  import Test.Hspec

  import Numeric.YHSeq.V0200.Type
  import Numeric.YHSeq.V0200.Compression (compress)
  import Numeric.YHSeq.V0200.Decompression

  spec :: Spec
  spec = do

    describe "decompress" $ do

      it "is a inverse function of `compress` at (1,2,4,8,10,8)" $ do
        decompress (compress [1,2,4,8,10,8]) `shouldBe` [1,2,4,8,10,8]
