import Prelude
import Test.Hspec

import qualified Numeric.YHSeq.V0110 as V0110 (fseq)

main :: IO ()
main = hspec $ do
  describe "V0110.fseq" $ do
    it "expand (1,2) to (1,1,1,...)" $ do
      V0110.fseq [1,2] 2 `shouldBe` [1,1,1]

    it "expand (1,2,4,5,4) to (1,2,4,5,3,5,6,4,6,7,...)" $ do
      V0110.fseq [1,2,4,5,4] 2 `shouldBe` [1,2,4,5,3,5,6,4,6,7]

    it "expand (1,2,4,6) to (1,2,4,5,7,8,10,...)" $ do
      V0110.fseq [1,2,4,6] 2 `shouldBe` [1,2,4,5,7,8,10]

    it "expand (1,2,4,8,9,8) to (1,2,4,8,9,7,12,13,11,17,18,...)" $ do
      V0110.fseq [1,2,4,8,9,8] 2 `shouldBe` [1,2,4,8,9,7,12,13,11,17,18]

    it "expand (1,2,4,8,10,7) to (1,2,4,8,10,6,10,12,8,12,14,...)" $ do
      V0110.fseq [1,2,4,8,10,7] 2 `shouldBe` [1,2,4,8,10,6,10,12,8,12,14]

    it "expand (1,2,4,8,10,8) to (1,2,4,8,10,7,12,15,11,17,21,...)" $ do
      V0110.fseq [1,2,4,8,10,8] 2 `shouldBe` [1,2,4,8,10,7,12,15,11,17,21]
