module Main where

  import Prelude

  import Control.Exception (ErrorCall, catch, evaluate, throwIO)

  import System.Environment (getArgs)

  import Numeric.YHSeq.V0201 (expandList)

  type OpArg = (String, String)

  type Arg = ([Int], Int)

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
    s <- evaluate (read seq :: [Int])
    n <- evaluate (read num :: Int)
    return (s, n)

  calc :: Arg -> IO [Int]
  calc (seq, num) = case expandList seq num of
    Left e -> evaluate (error (show e))
    Right x -> return x
