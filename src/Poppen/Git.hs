-- | Allows us to anwser questions and do action to git repositories
module Poppen.Git
  ( isBranchDirty
  )
where

import Toml
import Toml.FromValue
import GHC.Generics
import Toml.FromValue.Generic
import Data.Foldable (traverse_)
import Data.Map
import Git
import Poppen.Toml
import Git.Libgit2

repositoryFactory = RepositoryFactory {
  openRepository = openLgRepository
  , runRepository = runLgRepository
  }

mkOptions :: FilePath -> Project -> RepositoryOptions
mkOptions projectDir project =
  defaultRepositoryOptions { repoPath = projectDir </> path project }

isBranchDirty :: FilePath -> Project -> IO Bool
isBranchDirty projectDir project =
  withRepository repositoryFactory (mkOptions projectDir project) $ do
