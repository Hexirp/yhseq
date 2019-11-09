module YHSeq where

  import Prelude

  -- YHSeq + Arguments
  data YHSA = YHSA
    { yhsv :: String -- version
    , yhss :: String -- sequence
    , yhsn :: String -- number
    , yhsd :: Bool -- detail
    , yhsf :: Bool -- force
    }

  yhseq :: YHSA -> IO ()
  yhseq = return ()
