module YHSeq where

  import Prelude

  data Arguments = Arguments {
    version :: String ,
    seq :: String ,
    num :: String }

  yhseq :: Argments -> IO ()
  yhseq = return ()
