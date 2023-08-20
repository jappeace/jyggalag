module Poppen.Copy
  ( commandCopy
  , CopyOptions(..)
  )
where

import qualified Poppen.Toml as Toml
import qualified Data.Map as Map
import System.Directory (listDirectory, createDirectoryIfMissing, copyFile)
import System.FilePath ((</>))
import Control.Monad (forM_)
import qualified Poppen.Git as Git

data CopyOptions = CopyOptions {
  configFile :: FilePath
  }

workflowPath :: FilePath
workflowPath = ".github" </> "workflows"

commandCopy :: CopyOptions -> IO ()
commandCopy (CopyOptions path) = do
  configFile <- Toml.parseConfig path

  actionsToBeCopied <- listDirectory $ Toml.actionsPath configFile

  forM_ (Map.toList $ Toml.projects configFile) $ \(projectName, project) ->  do
      isDirty <- Git.isBranchDirty (Toml.projectDir configFile) project
      if isDirty then
        putStrLn $ "skipping project " <> show projectName <> " because branch is dirty"
      else do
        Git.setWorkBranch (Toml.projectDir configFile) project $ Toml.workbranch configFile
        forM_ actionsToBeCopied $ \action -> do
          copyAction configFile action projectName project

copyAction :: Toml.ConfigFile -> FilePath -> Toml.ProjectName -> Toml.Project -> IO ()
copyAction configFile action projectName project = do
  putStrLn $ "copying over " <> action <> " into " <> show projectName
  let fromPath = Toml.actionsPath configFile </> action
  let projectActionsPath = Toml.projectDir configFile </> Toml.path project </> workflowPath
  createDirectoryIfMissing True projectActionsPath
  if elem action $ Toml.ignoreActions project then
    putStrLn $ "ignoring " <> action <> " for " <> show projectName
  else copyFile fromPath $ projectActionsPath </> action
