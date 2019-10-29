import Prelude
import Test.Hspec

import qualified Numeric.YHSeq.V0110.Compression as V0110 ( mtD
                                                          , mtP
                                                          , nonEmptyDepth
                                                          )
import qualified Numeric.YHSeq.V0110             as V0110 (fseq)

main :: IO ()
main = hspec $ do

  describe "V0110/mountain" $ do

    it "is the same as the calculation result by hand" $ do
      V0110.mtD [1,2] 1 1 `shouldBe` 1

    it "is the same as the calculation result by hand" $ do
      V0110.mtD [1,2] 2 1 `shouldBe` 2

    it "is the same as the calculation result by hand" $ do
      V0110.mtD [1,2] 1 2 `shouldBe` 0

    it "is the same as the calculation result by hand" $ do
      V0110.mtD [1,2] 2 2 `shouldBe` 1

    it "is the same as the calculation result by hand" $ do
      V0110.mtD [1,2] 1 3 `shouldBe` 0

    it "is the same as the calculation result by hand" $ do
      V0110.mtD [1,2] 2 3 `shouldBe` 0

    it "is the same as the calculation result by hand" $ do
      V0110.mtP [1,2] 1 1 `shouldBe` 0

    it "is the same as the calculation result by hand" $ do
      V0110.mtP [1,2] 2 1 `shouldBe` 1

    it "is the same as the calculation result by hand" $ do
      V0110.mtP [1,2] 1 2 `shouldBe` 0

    it "is the same as the calculation result by hand" $ do
      V0110.mtP [1,2] 2 2 `shouldBe` 1

    it "is the same as the calculation result by hand" $ do
      V0110.mtP [1,2] 1 3 `shouldBe` 0

    it "is the same as the calculation result by hand" $ do
      V0110.mtP [1,2] 2 3 `shouldBe` 0

  describe "V0110.nonEmptyDepth" $ do

    it "returns 2 at (1,2)" $ do
      V0110.nonEmptyDepth [1,2] `shouldBe` 2

    it "returns 3 at (1,2,4)" $ do
      V0110.nonEmptyDepth [1,2,4] `shouldBe` 3

    it "returns 3 at (1,2,4,8,10,7)" $ do
      V0110.nonEmptyDepth [1,2,4,8,10,7] `shouldBe` 3

    it "returns 4 at (1,2,4,8,10,8)" $ do
      V0110.nonEmptyDepth [1,2,4,8,10,8] `shouldBe` 4

  describe "V0110.fseq" $ do

    it "expands (1,2) to (1,1,1,...)" $ do
      V0110.fseq [1,2] 2 `shouldBe` [1,1,1]

    it "expands (1,2,4,5,4) to (1,2,4,5,3,5,6,4,6,7,...)" $ do
      V0110.fseq [1,2,4,5,4] 2 `shouldBe` [1,2,4,5,3,5,6,4,6,7]

    it "expands (1,2,4,6) to (1,2,4,5,7,8,10,...)" $ do
      V0110.fseq [1,2,4,6] 2 `shouldBe` [1,2,4,5,7,8,10]

    it "expands (1,2,4,8,9,8) to (1,2,4,8,9,7,12,13,11,17,18,...)" $ do
      V0110.fseq [1,2,4,8,9,8] 2 `shouldBe` [1,2,4,8,9,7,12,13,11,17,18]

    it "expands (1,2,4,8,10,7) to (1,2,4,8,10,6,10,12,8,12,14,...)" $ do
      V0110.fseq [1,2,4,8,10,7] 2 `shouldBe` [1,2,4,8,10,6,10,12,8,12,14]

    it "expands (1,2,4,8,10,8) to (1,2,4,8,10,7,12,15,11,17,21,...)" $ do
      V0110.fseq [1,2,4,8,10,8] 2 `shouldBe` [1,2,4,8,10,7,12,15,11,17,21]
