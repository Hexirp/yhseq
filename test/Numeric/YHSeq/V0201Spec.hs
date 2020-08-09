module Numeric.YHSeq.V0201Spec (spec) where

  import Prelude

  calcMtFromList :: [Int] -> Mountain
  calcMtFromList x = calcMt (Sequence (V.fromList x))

  spec :: Spec
  spec = runIO $ do
    print "calcMt $ Sequence $ V.fromList $ [1,2,4,8,10,8]"
    print (calcMt $ Sequence $ V.fromList $ [1,2,4,8,10,8])
    print "mtClass $ calcMtFromList []"
    print (mtClass $ calcMtFromList [])
    print "mtClass $ calcMtFromList [1,2,1]"
    print (mtClass $ calcMtFromList [1,2,1])
    print "mtClass $ calcMtFromList [1,2,4,8,10,8]"
    print (mtClass $ calcMtFromList [1,2,4,8,10,8])
    print "mtClass $ calcMtFromList [1,3]"
    print (mtClass $ calcMtFromList [1,3])
    print "mtClass $ calcMtFromList [1,3,4,2,5,6,5]"
    print (mtClass $ calcMtFromList [1,3,4,2,5,6,5])
    print "mtClass $ calcMtFromList [1,11]"
    print (mtClass $ calcMtFromList [1,11])
    print "calcDpn $ calcMtFromList [1,2,3,8,10,8]"
    print (calcDpn $ calcMtFromList [1,2,3,8,10,8])
