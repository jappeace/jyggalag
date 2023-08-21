-- | Wraps the toml-parser library to create a ConfigFile
module Poppen.Toml
  ( parseConfig
  , Project(..)
  , ConfigFile(..)
  , ProjectName(..)
  , actionsPath
  , Branch
  , unBranch
  )
where

import Toml
import Toml.FromValue
import GHC.Generics
import Toml.FromValue.Generic
import Data.Foldable (traverse_)
import Data.Map
import System.FilePath ((</>))
import Data.Time (UTCTime)
import Data.Time.Format (formatTime)
import Data.Time (defaultTimeLocale)

actionsPath :: ConfigFile -> FilePath
actionsPath configFile =
  templateProject </> ".github" </> "workflows"
  where
    templateProject = projectDir configFile </> template configFile



data Project = MkProject {
    path :: FilePath
  , ignoreActions :: [FilePath]
  } deriving (Generic, Show)

newtype ProjectName = MkProjectName String
  deriving newtype (Eq, Ord, Show)

newtype Branch = MkBranch String
  deriving Show

unBranch :: Branch -> UTCTime -> String
unBranch (MkBranch branch) time' = branch <> "_" <> formatTime defaultTimeLocale "%Y_%m_%d__%H_%M_%S" time'

data ConfigFile = MkConfigFile {
    projectDir :: FilePath
  , template :: FilePath
  , projects :: Map ProjectName Project
  , workbranch :: Branch
  } deriving (Generic, Show)


instance FromKey ProjectName where
  fromKey = fmap MkProjectName <$> fromKey

instance FromValue Branch where
  fromValue = fmap MkBranch <$> fromValue

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
