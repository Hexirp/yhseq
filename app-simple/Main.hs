module Main where

  import Prelude
  import System.Environment (getArgs)
  import System.Exception (ErrorCall, try, evaluate, throwIO)

  import Numeric.YHSeq.V0210 (fseq)

  type OpArg = (String, String)

  type Arg = ([Integer], Integer)

  main :: IO ()
  main = do
    args <- getArgs
    opArg <- optparse args
    arg <- parse opArg
    e <- (try $ calc arg) :: IO (Either ErrorCall [Integer])
    case e of
      Left ex -> throwIO ex
      Right s' -> print s'

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
