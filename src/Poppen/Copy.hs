module Poppen.Copy
  ( commandCopy
  , CopyOptions(..)
  )
where

import Poppen.Toml

data CopyOptions = CopyOptions {
  configFile :: FilePath
  }


commandCopy :: CopyOptions -> IO ()
commandCopy (CopyOptions path) = do
  configFile <- parseConfig path
  print configFile
