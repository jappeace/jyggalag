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
import Jyggalag.Git (GitContext)
import Data.Maybe (fromMaybe)
import UnliftIO (bracket_)

data CopyOptions = CopyOptions {
  configFile :: FilePath
  }

workflowPath :: FilePath
workflowPath = ".github" </> "workflows"

commandCopy :: CopyOptions -> IO ()
commandCopy (CopyOptions path) = do
  configFile <- Toml.parseConfig path

  actionsToBeCopied <- listDirectory $ Toml.actionsPath configFile

  uris <- forM (Map.toList $ Toml.projects configFile) $ \(projectName, project) ->  do
      gitContext <- Git.createGitContext (Toml.projectDir configFile) project
      isDirty <- Git.isBranchDirty gitContext
      if isDirty then
        pure $ "skipping project " <> Text.pack (show projectName) <> " because branch is dirty"
      else do
        Git.revertSetBranch gitContext $ fromMaybe Toml.defaultRevertBranch $ Toml.revertBranch project
        Git.pull gitContext
        bracket_ (Git.setWorkBranch gitContext $ Toml.workbranch configFile)
                 -- revert cuz it's rather annoying everything stays on those jyggalag branches
                 (Git.revertSetBranch gitContext $ fromMaybe Toml.defaultRevertBranch $ Toml.revertBranch project) $ do
          forM_ actionsToBeCopied $ \action -> do
            copyAction configFile gitContext action projectName project
          Git.commit gitContext
          Git.push gitContext $ Toml.workbranch configFile

  traverse_ Text.putStrLn uris

copyAction :: Toml.ConfigFile -> GitContext -> FilePath -> Toml.ProjectName -> Toml.Project -> IO ()
copyAction configFile gitContext action projectName project = do
  putStrLn $ "copying over " <> action <> " into " <> show projectName
  let fromPath = Toml.actionsPath configFile </> action
  let projectActionsPath = Toml.projectDir configFile </> Toml.path project </> workflowPath
  createDirectoryIfMissing True projectActionsPath
  if elem action $ fold $ Toml.ignoreActions project then
    putStrLn $ "ignoring " <> action <> " for " <> show projectName
  else do
    copyFile fromPath $ projectActionsPath </> action
    Git.addStaging gitContext $ projectActionsPath </> action
