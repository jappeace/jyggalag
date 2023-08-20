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

data CopyOptions = CopyOptions {
  configFile :: FilePath
  }

workflowPath :: FilePath
workflowPath = ".github" </> "workflows"

commandCopy :: CopyOptions -> IO ()
commandCopy (CopyOptions path) = do
  configFile <- Toml.parseConfig path

  let templateProject = Toml.projectDir configFile </> Toml.template configFile
      actionsPath = templateProject </> ".github" </> "workflows"
  actionsToBeCopied <- listDirectory actionsPath

  forM_ actionsToBeCopied $ \action -> do
    forM_ (Map.toList $ Toml.projects configFile) $ \(projetName, project) ->  do
      isDirty <- Git.isBranchDirty (Toml.projectDir configFile) project
      if isDirty then
        putStrLn $ "skipping " <> show projetName <> " because branch is dirty"
      else
        copyAction (Toml.projectDir configFile) action projectName project

copyAction :: FilePath -> FilePath -> ProjectName -> Project -> IO ()
copyAction projectDir action projectName project = do
  putStrLn $ "copying over " <> action <> " into " <> show projetName
  let fromPath = actionsPath </> action
  let projectActionsPath = Toml.projectDir configFile </> Toml.path project </> workflowPath
  createDirectoryIfMissing True projectActionsPath
  if elem action $ Toml.ignoreActions project then
    putStrLn $ "ignoring " <> action <> " for " <> show projetName
  else copyFile fromPath $ projectActionsPath </> action
