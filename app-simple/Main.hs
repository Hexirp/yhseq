module Main where

  import Prelude
  import Numeric.YHSeq.V0210

  main :: IO ()
  main = do
    args <- getArgs
    print $ case args of
      [seq, num] -> fseq (read seq) (read num)
      _ -> error "The arguments are incorrect!"
