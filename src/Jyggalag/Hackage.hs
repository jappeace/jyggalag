module Jyggalag.Hackage
  ( commandUploadHackage
  , HackageOptions(..)
  )
where

import qualified Jyggalag.Toml as Toml
import qualified Data.Map as Map
import Jyggalag.Toml
import Options.Applicative
import System.Environment (getProgName)
import Data.Foldable

data HackageOptions = HackageOptions {
  configFile :: FilePath
  }

commandUploadHackage :: HackageOptions -> IO ()
commandUploadHackage (HackageOptions path) = do
  configFile <- Toml.parseConfig path
  let projecList = Map.toList (projects configFile)
  _projectAnwsers <- traverse withProject projecList
  putStrLn "totally uploading"

data ShouldUpload = Upload Project
                  | Skip

parseOptions :: ProjectName -> Project -> Parser ShouldUpload
parseOptions name' project = hsubparser $
  fold [
  command "y"
    (info (pure (Upload project))
      (progDesc $ "upload " <> show name'))
  , command "n"
    (info (pure Skip)
     (progDesc $ "skip " <> show name' ))
  ]

withProject :: (ProjectName, Project) -> IO ShouldUpload
withProject (name, project) = do
  putStrLn $ "should upload " <> show name <> "? (y = upload/n = skip)"
  line'  <- getLine
  let  result :: ParserResult ShouldUpload
       result = execParserPure (prefs showHelpOnError) (info
              (helper <*> (parseOptions name project))
              (fullDesc <> Options.Applicative.header "Jyggalag" <> progDesc
                "Helps managing many opensoure project by standardization"
              )) $ words line'

  case result of
      Success x -> pure x
      Failure failure -> do
        progn <- getProgName
        let (msg, _exit) = renderFailure failure progn
        putStrLn msg
        withProject (name, project)
      (CompletionInvoked compl) -> do
        progn <- getProgName
        msg <- execCompletion compl progn
        putStrLn msg
        withProject (name, project)
