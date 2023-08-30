{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}

-- | Allows us to anwser questions and do action to git repositories
module Jyggalag.Git
  ( isBranchDirty
  , addStaging
  , pull
  , commit
  , push
  , GitContext
  , MonadGit
  , withGit
  , workOnBranch
  , checkout
  , runGitStdOut
  , runGit_
  , runGitExitCode
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
import Control.Monad.Reader
import UnliftIO (MonadUnliftIO)
import Data.ByteString.Lazy(ByteString)
import UnliftIO.Exception (bracket_)
import Data.Maybe (fromMaybe)

data GitContext = MkGitContext {
    projectDir :: FilePath
  , project :: Project
  , creationTime :: UTCTime
  }

newtype MonadGit a = MonadGit { runMonadGit :: (ReaderT GitContext IO) a }
  deriving newtype (Functor, Applicative, Monad,
                    MonadUnliftIO,
                    MonadIO,
                    MonadReader GitContext)


withGit :: FilePath -> Project -> MonadGit a -> IO a
withGit projectDir project someMonad = do
  ctx <- createGitContext projectDir project
  runReaderT (runMonadGit someMonad) ctx

runGit_ :: ProcessConfig stdin stdout stderr -> MonadGit ()
runGit_ x = () <$ runGitExitCode x

runGitStdOut :: ProcessConfig stdin stdout stderr -> MonadGit ByteString
runGitStdOut processConfig = do
  MkGitContext {..} <- ask
  readProcessStdout_ $ setWorkingDir (projectDir </> path project) processConfig

runGitExitCode :: ProcessConfig stdin stdout stderr -> MonadGit ExitCode
runGitExitCode processConfig = do
  MkGitContext {..} <- ask
  runProcess $ setWorkingDir (projectDir </> path project) processConfig

createGitContext :: FilePath -> Project -> IO GitContext
createGitContext projectDir project = do
  creationTime <- getCurrentTime
  pure $ MkGitContext {..}

isBranchDirty :: MonadGit Bool
isBranchDirty = do
  stdOut <- runGitStdOut $ shell "git status --porcelain=v2"
  pure $ stdOut /= mempty

workOnBranch :: Branch -> MonadGit a -> MonadGit a
workOnBranch branch gitSpells = do
  MkGitContext {..} <- ask
  bracket_ (setWorkBranch branch) (checkout $ fromMaybe defaultRevertBranch $ revertBranch project) gitSpells

-- | This creates a branch
setWorkBranch :: Branch -> MonadGit ()
setWorkBranch branch = do
  MkGitContext {..} <- ask
  runGit_ $ shell ("git checkout -B \"" <> formatTimeBranch branch creationTime <> "\" master")

-- TOOD this shouldn't be a branch but any tag
checkout :: Branch -> MonadGit ()
checkout branch =
  runGit_ $ shell ("git checkout \"" <> branchToString branch <> "\"")

pull :: Branch -> MonadGit ExitCode
pull branch = do
  runGitExitCode
      $ shell ("git pull --ff-only origin " <> branchToString branch)

addStaging :: FilePath -> MonadGit ()
addStaging file =
  runGit_ $ shell $ "git add \"" <> file <> "\""


commit :: MonadGit ()
commit =
  runGit_ $ shell "git commit -m \"jyggalag update\n\n See https://github.com/jappeace/jyggalag\""

push :: Branch -> MonadGit Text
push branch = do
  MkGitContext {..} <- ask
  runGit_ $ shell ("git push --set-upstream origin " <> formatTimeBranch branch creationTime)
  stdOut <- runGitStdOut $ shell "git remote -v"

  let uri = Text.takeWhile (/= '.') $ Text.drop 1 $ Text.dropWhile (/= ':') $ decodeUtf8 $ toStrict stdOut

  pure $ "https://github.com/" <> uri  <> "/pull/new/" <> pack (formatTimeBranch branch creationTime)
