{-# LANGUAGE RecordWildCards #-}
-- | Allows us to anwser questions and do action to git repositories
module Jyggalag.Git
  ( isBranchDirty
  , setWorkBranch
  , addStaging
  , createGitContext
  , pull
  , commit
  , push
  , GitContext
  , revertSetBranch
  )
where

import Jyggalag.Toml
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

-- | This creates a branch
setWorkBranch :: GitContext -> Branch -> IO ()
setWorkBranch MkGitContext {..} branch = do
  runProcess_
      $ setWorkingDir (projectDir </> path project)
      $ shell ("git checkout -B \"" <> formatTimeBranch branch creationTime <> "\" master")

revertSetBranch :: GitContext -> Branch -> IO ()
revertSetBranch MkGitContext {..} branch = do
  runProcess_
      $ setWorkingDir (projectDir </> path project)
      $ shell ("git checkout \"" <> branchToString branch <> "\"")

pull :: GitContext -> IO ()
pull MkGitContext {..} = do
  runProcess_
      $ setWorkingDir (projectDir </> path project)
      $ shell ("git pull")

addStaging :: GitContext -> FilePath -> IO ()
addStaging MkGitContext {..} file =
  runProcess_
      $ setWorkingDir (projectDir </> path project)
      $ shell $ "git add \"" <> file <> "\""


commit :: GitContext -> IO ()
commit MkGitContext {..} =
  runProcess_
      $ setWorkingDir (projectDir </> path project)
      $ shell "git commit -m \"jyggalag update\n\n See https://github.com/jappeace/jyggalag\""

push :: GitContext -> Branch -> IO Text
push MkGitContext {..} branch = do
  runProcess_
      $ setWorkingDir (projectDir </> path project)
      $ shell ("git push --set-upstream origin " <> formatTimeBranch branch creationTime)

  stdOut <- readProcessStdout_
      $ setWorkingDir (projectDir </> path project)
      $ shell "git remote -v"

  let uri = Text.takeWhile (/= '.') $ Text.drop 1 $ Text.dropWhile (/= ':') $ decodeUtf8 $ toStrict stdOut

  pure $ "https://github.com/" <> uri  <> "/pull/new/" <> pack (formatTimeBranch branch creationTime)
