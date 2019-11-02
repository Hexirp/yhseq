module Numeric.YHSeq.V0200.ExpansionSpec (spec) where

  import Prelude hiding (length)

  import Test.Hspec

  import Numeric.YHSeq.V0200.Type
  import Numeric.YHSeq.V0200.Expansion

  spec :: Spec
  spec = do

    describe "goodPart" $ do

      it "is (1) at (1,2,4)" $ do
        goodPart [1,2,4] `shouldBe` [1]
