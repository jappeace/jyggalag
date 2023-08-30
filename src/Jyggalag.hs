module Jyggalag
  ( main
  )
where

import Options.Applicative
import Jyggalag.Copy
import Data.Foldable (fold)
import Jyggalag.Hackage

data Commands = CopyOverActions CopyOptions
              | UploadHackage HackageOptions

notifyRun :: Commands -> IO ()
notifyRun = \case
  CopyOverActions _ -> putStrLn "running copy over github actions"
  UploadHackage _ -> putStrLn "running upload hackage"

parseConfigFile :: Parser FilePath
parseConfigFile = strOption $ long "config" <> metavar "FILE" <> help "the configuration file" <> value "jyggalag.toml"

parseOptions :: Parser Commands
parseOptions = hsubparser $
  fold [

  command "copy"
    (info (CopyOverActions . CopyOptions  <$> parseConfigFile )
      (progDesc "copy over the actions in the managed projects"))
  , command "upload-hackage"
    (info (UploadHackage . HackageOptions <$> parseConfigFile)
     (progDesc "goes over your projects, ask one by one if they need to be published, and does so, use `cabal user-config update` to find out the file to put in your username and password ~/.cabal/config on my machine."))
  ]

main :: IO ()
main = do
  commands <- readCliOptions
  notifyRun commands
  case commands of
    CopyOverActions copyOptions -> commandCopy copyOptions
    UploadHackage options -> commandUploadHackage options

readCliOptions :: IO Commands
readCliOptions = do
  customExecParser (prefs showHelpOnError) $ info
    (helper <*> parseOptions)
    (fullDesc <> Options.Applicative.header "Jyggalag" <> progDesc
      "Helps managing many opensoure project by standardization"
    )
