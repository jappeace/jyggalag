module Jyggalag.Copy
  ( commandCopy
  , CopyOptions(..)
  )
where

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
        Git.setWorkBranch gitContext $ Toml.workbranch configFile
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
  if elem action $ Toml.ignoreActions project then
    putStrLn $ "ignoring " <> action <> " for " <> show projectName
  else do
    copyFile fromPath $ projectActionsPath </> action
    Git.addStaging gitContext $ projectActionsPath </> action
