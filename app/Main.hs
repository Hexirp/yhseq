module Main where

  import Prelude
  import Options.Applicative

  data Config = Config
    { version :: String
    , sequence :: [Integer]
    , number :: Integer
    , withVersionInfo :: Bool
    , withDetail :: Bool
    , forcing :: Bool
    }

  optparse :: Parser Config
  optparse = Config
    <$> argument auto (metavar "VERSION")
    <*> argument auto (metavar "SEQ")
    <*> argument auto (metavar "NUM")
    <*> switch
      ( mempty
      <> long "version"
      <> short 'v'
      <> help "Print the command's version"
      )
    <*> switch
      ( mempty
      <> long "detail"
      <> short 'd'
      <> help "Print details"
      )
    <*> switch
      ( mempty
      <> long "force"
      <> short 'f'
      <> help "Force to use deprecated versions"
      )

  main :: IO ()
  main = do
      conf <- execParser opts
      fooApp conf
    where
      opts = info (flip ($) <$> optparse <*> helper)
        ( mempty
        <> progDesc "Print a result of foo"
        <> header "foo - a command for foo"
        )

  fooApp :: Config -> IO ()
  fooApp = undefined
