import Prelude
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "V0110.fseq" $ do
    it "expand (1,2) to (1,1,1,...)" $ do
      fseq [1,2] 2 `shouldBe` [1,1,1]

    it "expand (1,2,4,5,4) to (1,2,4,5,3,5,6,4,6,7,...)" $ do
      fseq [1,2,4,5,4] 2 `shouldBe` [1,2,4,5,3,5,6,4,6,7]

    it "expand (1,2,4,6) to (1,2,4,5,7,8,10,...)" $ do
      fseq [1,2,4,6] 2 `shouldBe` [1,2,4,5,7,8,10]

    it "expand (1,2,4,8,10,7) to (1,2,4,8,10,6,10,12,8,12,14,...)" $ do
      fseq [1,2,4,8,10,7] 2 `shouldBe` [1,2,4,8,10,6,10,12,8,12,14]

    it "expand (1,2,4,8,10,8) to (1,2,4,8,10,7,12,15,11,17,21,...)" $ do
      fseq [1,2,4,8,10,8] 2 `shouldBe` [1,2,4,8,10,7,12,15,11,17,21]
