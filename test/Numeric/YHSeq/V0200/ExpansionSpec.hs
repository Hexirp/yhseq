module Numeric.YHSeq.V0200.ExpansionSpec (spec) where

  import Prelude hiding (length)

  import Test.Hspec

  import Numeric.YHSeq.V0200.Type
  import Numeric.YHSeq.V0200.Compression (compress)
  import Numeric.YHSeq.V0200.Expansion

  spec :: Spec
  spec = do

    describe "badRoot" $ do

      it "is 2 at (1,2,4,5,4)" $ do
        badRoot (compress [1,2,4,5,4]) `shouldBe` 2
