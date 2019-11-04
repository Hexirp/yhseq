module Numeric.YHSeq.V0210Spec (spec) where

  import Prelude hiding (length)

  import Test.Hspec

  import Numeric.YHSeq.V0210

  spec :: Spec
  spec = do

    describe "fseq" $ do

      it "expands (1,2) to (1,1,1,...)" $ do
        fseq [1,2] 2 `shouldBe` [1,1,1]

      it "expands (1,2,4,5,4) to (1,2,4,5,3,5,6,4,6,7,...)" $ do
        fseq [1,2,4,5,4] 2 `shouldBe` [1,2,4,5,3,5,6,4,6,7]

      it "expands (1,2,4,6) to (1,2,4,5,7,8,10,...)" $ do
        fseq [1,2,4,6] 2 `shouldBe` [1,2,4,5,7,8,10]

      it "expands (1,2,4,8,9,8) to (1,2,4,8,9,7,12,13,11,17,18,...)" $ do
        fseq [1,2,4,8,9,8] 2 `shouldBe` [1,2,4,8,9,7,12,13,11,17,18]

      it "expands (1,2,4,8,10,7) to (1,2,4,8,10,6,10,12,8,12,14,...)" $ do
        fseq [1,2,4,8,10,7] 2 `shouldBe` [1,2,4,8,10,6,10,12,8,12,14]

      it "expands (1,2,4,8,10,8) to (1,2,4,8,10,7,12,15,11,17,21,...)" $ do
        fseq [1,2,4,8,10,8] 2 `shouldBe` [1,2,4,8,10,7,12,15,11,17,21]
