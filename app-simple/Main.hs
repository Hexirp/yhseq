module Main where

  import Prelude
  import System.Environment (getArgs)

  import Numeric.YHSeq.V0210 (fseq)

  main :: IO ()
  main = do
    args <- getArgs
    print $ case args of
      [seq, num] -> fseq (read seq) (read num)
      _ -> error "The arguments are incorrect!"
