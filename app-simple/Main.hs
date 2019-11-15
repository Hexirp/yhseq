module Main where

  import Prelude

  import Control.Exception (ErrorCall, try, evaluate, throwIO)

  import System.Environment (getArgs)

  import Numeric.YHSeq.V0210 (fseq)

  type OpArg = (String, String)

  type Arg = ([Integer], Integer)

  main :: IO ()
  main = do
    args <- getArgs
    opArg <- optparse args
    arg <- parse opArg
    res <- calc arg `catch` \e -> const id (e :: ErrorCall) $ do
      putStrLn "yhseq-simple: Something happened!"
      throwIO e
    print res

  optparse :: [String] -> IO OpArg
  optparse [seq, num] = return (seq, num)
  optparse _          = evaluate $ error "The arguments is incorrect!"

  parse :: OpArg -> IO Arg
  parse (seq, num) = do
    s <- evaluate $ (read seq :: [Integer])
    n <- evaluate $ (read num :: Integer)
    return $ (s, n)

  calc :: Arg -> IO [Integer]
  calc (seq, num) = evaluate $ fseq seq num
