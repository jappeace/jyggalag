module Poppen
  ( main
  )
where

import Options.Applicative

data CopyOptions = CopyOptions {
  configFile :: FilePath
  }

data Commands = NoOp
              | CopyOverActions CopyOptions

parseConfigFile :: Parser FilePath
parseConfigFile = strOption $ long "config" <> metavar "FILE" <> help "the configuration file" <> value "poppen.toml"

parseOptions :: Parser Commands
parseOptions = hsubparser $
  fold [
  command "no-op"
    (info (pure NoOp)
      (progDesc "does nothing"))
  ,
  command "copy"
    (info (CopyOverActions <$> parseConfigFile )
      (progDesc "copy over the actions in the managed projects"))
  ]

main :: IO ()
main = putStrLn "hello, world flaky"

readCliOptions :: IO CliOptions
readCliOptions = do
  customExecParser (prefs showHelpOnError) $ info
    (helper <*> parseOptions)
    (fullDesc <> Options.Applicative.header "Poppen" <> progDesc
      "Helps managing many opensoure project by standardization"
    )
