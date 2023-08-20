module Poppen
  ( main
  )
where

import Options.Applicative
import Poppen.Copy
import Data.Foldable (fold)

data Commands = NoOp
              | CopyOverActions CopyOptions

notifyRun :: Commands -> IO ()
notifyRun = \case
  NoOp -> putStrLn ""
  CopyOverActions _ -> putStrLn "running copy over github actions"

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
    (info (CopyOverActions . CopyOptions  <$> parseConfigFile )
      (progDesc "copy over the actions in the managed projects"))
  ]

main :: IO ()
main = do
  commands <- readCliOptions
  notifyRun commands
  case commands of
    CopyOverActions copyOptions -> commandCopy copyOptions
    NoOp -> putStrLn "do nothing"


readCliOptions :: IO Commands
readCliOptions = do
  customExecParser (prefs showHelpOnError) $ info
    (helper <*> parseOptions)
    (fullDesc <> Options.Applicative.header "Poppen" <> progDesc
      "Helps managing many opensoure project by standardization"
    )
