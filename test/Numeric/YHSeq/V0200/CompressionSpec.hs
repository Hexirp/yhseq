module Numeric.YHSeq.V0200.CompressionSpec (spec) where

  import Prelude hiding (length)

  import Test.Hspec

  import Numeric.YHSeq.V0200.Type
  import Numeric.YHSeq.V0200.Compression

  spec :: Spec
  spec = do

    describe "mtD" $ do

      it "is D = 1 at S = (1,2), x = 1, and n = 1" $ do
        mtD [1,2] 1 1 `shouldBe` 1

      it "is D = 2 at S = (1,2), x = 2, and n = 1" $ do
        mtD [1,2] 2 1 `shouldBe` 2

      it "is D = 0 at S = (1,2), x = 1, and n = 2" $ do
        mtD [1,2] 1 2 `shouldBe` 0

      it "is D = 1 at S = (1,2), x = 2, and n = 2" $ do
        mtD [1,2] 2 2 `shouldBe` 1

      it "is D = 0 at S = (1,2), x = 1, and n = 3" $ do
        mtD [1,2] 1 3 `shouldBe` 0

      it "is D = 0 at S = (1,2), x = 2, and n = 3" $ do
        mtD [1,2] 2 3 `shouldBe` 0

      context "when ignored branches exist" $ do

        it "is D = 10 at S = (1,2,4,8,10,8), x = 5, and n = 1" $ do
          mtD [1,2,4,8,10,8] 5 1 `shouldBe` 10

        it "is D = 2 at S = (1,2,4,8,10,8), x = 5, and n = 2" $ do
          mtD [1,2,4,8,10,8] 5 2 `shouldBe` 2

        it "is D = 1 at S = (1,2,4,8,10,8), x = 5, and n = 3" $ do
          mtD [1,2,4,8,10,8] 5 3 `shouldBe` 1

    describe "mtP" $ do

      it "is P = 0 at S = (1,2), x = 1, and n = 1" $ do
        mtP [1,2] 1 1 `shouldBe` 0

      it "is P = 1 at S = (1,2), x = 2, and n = 1" $ do
        mtP [1,2] 2 1 `shouldBe` 1

      it "is P = 0 at S = (1,2), x = 1, and n = 2" $ do
        mtP [1,2] 1 2 `shouldBe` 0

      it "is P = 1 at S = (1,2), x = 2, and n = 2" $ do
        mtP [1,2] 2 2 `shouldBe` 1

      it "is P = 0 at S = (1,2), x = 1, and n = 3" $ do
        mtP [1,2] 1 3 `shouldBe` 0

      it "is P = 0 at S = (1,2), x = 2, and n = 3" $ do
        mtP [1,2] 2 3 `shouldBe` 0

      context "when ignored branches exist" $ do

        it "is P = 4 at S = (1,2,4,8,10,8), x = 5, and n = 1" $ do
          mtP [1,2,4,8,10,8] 5 1 `shouldBe` 4

        it "is P = 2 at S = (1,2,4,8,10,8), x = 5, and n = 2" $ do
          mtP [1,2,4,8,10,8] 5 2 `shouldBe` 2

        it "is P = 2 at S = (1,2,4,8,10,8), x = 5, and n = 3" $ do
          mtP [1,2,4,8,10,8] 5 3 `shouldBe` 2

    describe "compress" $ do

      it "is equal to the hand-calculated at (1,2,4,5,4)" $ do
        compress [1,2,4,5,4] `shouldBe`
          [ (1, [0], 1)
          , (1, [1], 2)
          , (2, [2], 2)
          , (1, [3, 1], 2)
          , (2, [2], 2)
          ]
