{-# LANGUAGE RecordWildCards #-}
-- | Allows us to anwser questions and do action to git repositories
module Poppen.Git
  ( isBranchDirty
  , setWorkBranch
  , addStaging
  , createGitContext
  , commit
  , push
  , GitContext
  )
where

import Poppen.Toml
import System.Process.Typed
import System.FilePath ((</>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (toStrict)
import qualified Data.Text as Text
import Data.Text (pack)
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)

data GitContext = MkGitContext {
    projectDir :: FilePath
  , project :: Project
  , creationTime :: UTCTime
  }

createGitContext :: FilePath -> Project -> IO GitContext
createGitContext projectDir project = do
  creationTime <- getCurrentTime
  pure $ MkGitContext {..}

isBranchDirty :: GitContext -> IO Bool
isBranchDirty MkGitContext {..} = do
  stdOut <- readProcessStdout_
      $ setWorkingDir (projectDir </> path project)
      $ shell "git status --porcelain=v2"
  print stdOut
  pure $ stdOut /= mempty


setWorkBranch :: GitContext -> Branch -> IO ()
setWorkBranch MkGitContext {..} branch = do
  runProcess_
      $ setWorkingDir (projectDir </> path project)
      $ shell ("git checkout -B \"" <> unBranch branch creationTime <> "\" master")

addStaging :: GitContext -> FilePath -> IO ()
addStaging MkGitContext {..} file =
  runProcess_
      $ setWorkingDir (projectDir </> path project)
      $ shell $ "git add \"" <> file <> "\""


commit :: GitContext -> IO ()
commit MkGitContext {..} =
  runProcess_
      $ setWorkingDir (projectDir </> path project)
      $ shell "git commit -m \"jyggalag update\""

push :: GitContext -> Branch -> IO Text
push MkGitContext {..} branch = do
  runProcess_
      $ setWorkingDir (projectDir </> path project)
      $ shell ("git push --set-upstream origin " <> unBranch branch creationTime)

  stdOut <- readProcessStdout_
      $ setWorkingDir (projectDir </> path project)
      $ shell "git remote -v"

  let uri = Text.takeWhile (/= '.') $ Text.drop 1 $ Text.dropWhile (/= ':') $ decodeUtf8 $ toStrict stdOut

  pure $ "https://github.com/" <> uri  <> "/compare/" <> pack (unBranch branch creationTime) <> "?expand=1"
