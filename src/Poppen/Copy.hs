module Poppen.Copy
  ( commandCopy
  , CopyOptions(..)
  )
where

data CopyOptions = CopyOptions {
  configFile :: FilePath
  }


commandCopy :: CopyOptions -> IO ()
commandCopy _ = pure ()
