module YHSeq where

  import Prelude

  data Arguments = Arguments {
    version :: String ,
    seq     :: String ,
    num     :: String ,
    detail  :: Bool ,
    force   :: Bool }

  yhseq :: Arguments -> IO ()
  yhseq = return ()
