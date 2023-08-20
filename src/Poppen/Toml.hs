module Poppen.Toml
  ( parseConfig
  , Project(..)
  , ConfigFile(..)
  )
where

import Toml
import Toml.FromValue
import GHC.Generics
import Toml.FromValue.Generic
import Data.Foldable (traverse_)
import Data.Map

data Project = MkProject {
    path :: FilePath
  , ignoreActions :: FilePath
  } deriving (Generic, Show)

newtype ProjectName = MkProjectName String
  deriving Show
  deriving newtype (Eq, Ord)

data ConfigFile = MkConfigFile {
    projectDir :: FilePath
  , template :: FilePath
  , projects :: Map ProjectName Project
  } deriving (Generic, Show)

instance FromKey ProjectName where
  fromKey = fmap MkProjectName <$> fromKey

instance FromValue ConfigFile where
  fromValue = parseTableFromValue genericParseTable

instance FromValue Project where
  fromValue = parseTableFromValue genericParseTable

parseConfig :: FilePath -> IO ConfigFile
parseConfig filePath = do
  fileLines <- readFile filePath
  case decode fileLines of
    Failure errors -> do
      traverse_ putStrLn errors
      error (show errors)
    Success warnings result -> do
      traverse_ putStrLn warnings
      pure result
