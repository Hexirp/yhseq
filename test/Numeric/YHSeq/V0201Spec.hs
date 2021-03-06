module Numeric.YHSeq.V0201Spec (spec) where

  import Prelude

  import qualified Data.Vector as V

  import Numeric.YHSeq.V0201

  import Test.Hspec

  calcSeqFromList :: [Int] -> Sequence
  calcSeqFromList x = Sequence (V.fromList x)

  calcMtFromList :: [Int] -> Mountain
  calcMtFromList x = calcMtFromSeq (Sequence (V.fromList x))

  spec :: Spec
  spec = runIO $ do
    putStrLn ""
    putStrLn ""
    print "calcMtFromSeq $ Sequence $ V.fromList $ [1,2,4,8,10,8]"
    print (calcMtFromSeq $ Sequence $ V.fromList $ [1,2,4,8,10,8])
    print "calcCofType $ calcMtFromList []"
    print (calcCofType $ calcMtFromList [])
    print "calcCofType $ calcMtFromList [1,2,1]"
    print (calcCofType $ calcMtFromList [1,2,1])
    print "calcCofType $ calcMtFromList [1,2,4,8,10,8]"
    print (calcCofType $ calcMtFromList [1,2,4,8,10,8])
    print "calcCofType $ calcMtFromList [1,3]"
    print (calcCofType $ calcMtFromList [1,3])
    print "calcCofType $ calcMtFromList [1,3,4,2,5,6,5]"
    print (calcCofType $ calcMtFromList [1,3,4,2,5,6,5])
    print "calcCofType $ calcMtFromList [1,11]"
    print (calcCofType $ calcMtFromList [1,11])
    print "calcDpn $ calcMtFromList [1,2,4,8,10,8]"
    print (calcDpn $ calcMtFromList [1,2,4,8,10,8])
    print "expandMtAtLim1 (calcMtFromList [1,2]) 5"
    print (expandMtAtLim1 (calcMtFromList [1,2]) 5)
    print "expandMtAtLim1 (calcMtFromList [1,2.2]) 5"
    print (expandMtAtLim1 (calcMtFromList [1,2,2]) 5)
    print "expandMtAtLim1 (calcMtFromList [1,2,3]) 5"
    print (expandMtAtLim1 (calcMtFromList [1,2,3]) 5)
    print "expandMtAtLim1 (calcMtFromList [1,2,4]) 5"
    print (expandMtAtLim1 (calcMtFromList [1,2,4]) 5)
    print "expandSeq (calcSeqFromList [1,2,4,8,10,8]) 5"
    print (expandSeq (calcSeqFromList [1,2,4,8,10,8]) 5)
    print "expandList [1,2,4,8,10,8] 5"
    print (expandList [1,2,4,8,10,8] 5)
    print "expandList [1,3] 5"
    print (expandList [1,3] 5)
