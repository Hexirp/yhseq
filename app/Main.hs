module Main where

  import Prelude

  import Control.Exception (ErrorCall, catch, evaluate, throwIO)

  import System.Environment (getArgs)

  import Numeric.YHSeq.V0200 (fseq)

  type OpArg = (String, String)

  type Arg = ([Integer], Integer)

  main :: IO ()
  main = do
    args <- getArgs
    opArg <- optparse args
    arg <- parse opArg
    res <- calc arg `catch` \e -> let _e = e :: ErrorCall in do
      putStrLn "yhseq: Something happened!"
      throwIO e
    print res

  optparse :: [String] -> IO OpArg
  optparse [seq, num] = return (seq, num)
  optparse _          = evaluate (error "yhseq: The arguments is incorrect!")

  parse :: OpArg -> IO Arg
  parse (seq, num) = do
    s <- evaluate (read seq :: [Integer])
    n <- evaluate (read num :: Integer)
    return (s, n)

  calc :: Arg -> IO [Integer]
  calc (seq, num) = evaluate (fseq seq num)
