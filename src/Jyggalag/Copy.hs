module Jyggalag.Copy
  ( commandCopy
  , CopyOptions(..)
  )
where

import Data.Foldable(fold)
import qualified Jyggalag.Toml as Toml
import qualified Data.Map as Map
import System.Directory (listDirectory, createDirectoryIfMissing, copyFile)
import System.FilePath ((</>))
import Control.Monad (forM_)
import qualified Jyggalag.Git as Git
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Data.Foldable (traverse_)
import Data.Traversable (forM)
import Jyggalag.Git (MonadGit)
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode(..))
import Control.Monad.IO.Class

data CopyOptions = CopyOptions {
  configFile :: FilePath
  }

workflowPath :: FilePath
workflowPath = ".github" </> "workflows"

commandCopy :: CopyOptions -> IO ()
commandCopy (CopyOptions path) = do
  configFile <- Toml.parseConfig path

  actionsToBeCopied <- listDirectory $ Toml.actionsPath configFile

  uris <- forM (Map.toList $ Toml.projects configFile) $ \(projectName, project) ->
    Git.withGit (Toml.projectDir configFile) project $ do
      isDirty <- Git.isBranchDirty
      if isDirty then
        pure $ "skipping project " <> Text.pack (show projectName) <> " because branch is dirty"
      else do
        let defBranch = fromMaybe Toml.defaultRevertBranch $ Toml.revertBranch project
        Git.checkout defBranch
        exitCode <- Git.pull defBranch
        case exitCode of
          ExitFailure x -> pure $ "skipping project " <> Text.pack (show projectName) <> " because pull failed with " <> Text.pack (show x)
          ExitSuccess -> Git.workOnBranch (Toml.workbranch configFile) $ do
              forM_ actionsToBeCopied $ \action -> do
                copyAction configFile action projectName project
              Git.commit
              Git.push $ Toml.workbranch configFile

  traverse_ Text.putStrLn uris

copyAction :: Toml.ConfigFile -> FilePath -> Toml.ProjectName -> Toml.Project -> MonadGit ()
copyAction configFile action projectName project = do
  liftIO $ putStrLn $ "copying over " <> action <> " into " <> show projectName
  let fromPath = Toml.actionsPath configFile </> action
  let projectActionsPath = Toml.projectDir configFile </> Toml.path project </> workflowPath
  liftIO $ createDirectoryIfMissing True projectActionsPath
  if elem action $ fold $ Toml.ignoreActions project then
    liftIO $ putStrLn $ "ignoring " <> action <> " for " <> show projectName
  else do
    liftIO $ copyFile fromPath $ projectActionsPath </> action
    Git.addStaging $ projectActionsPath </> action
