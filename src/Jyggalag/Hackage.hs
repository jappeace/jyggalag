module Jyggalag.Hackage
  ( commandUploadHackage
  , HackageOptions(..)
  )
where

import qualified Jyggalag.Toml as Toml

data HackageOptions = HackageOptions {
  configFile :: FilePath
  }

commandUploadHackage :: HackageOptions -> IO ()
commandUploadHackage (HackageOptions path) = do
  _configFile <- Toml.parseConfig path
  putStrLn "totally uploading"
