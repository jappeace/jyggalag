-- | Allows us to anwser questions and do action to git repositories
module Poppen.Git
  ( isBranchDirty
  , setWorkBranch
  )
where

import Poppen.Toml
import System.Process.Typed
import System.FilePath ((</>))

isBranchDirty :: FilePath -> Project -> IO Bool
isBranchDirty projectDir project = do
  stdOut <- readProcessStdout_
      $ setWorkingDir (projectDir </> path project)
      $ shell "git status --porcelain=v2"
  pure $ stdOut /= mempty


setWorkBranch :: FilePath -> Project -> Branch -> IO ()
setWorkBranch projectDir project branch = do
  runProcess_
      $ setWorkingDir (projectDir </> path project)
      $ shell ("git checkout -B " <> unBranch branch)
