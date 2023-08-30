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
import Data.Text.Encoding
import Jyggalag.Git (runGitStdOut, withGit, runGitExitCode)
import System.Process.Typed
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text as Text
import Data.String
import Data.Maybe (listToMaybe)
import Control.Monad.IO.Class

data HackageOptions = HackageOptions {
  configFile :: FilePath
  }

commandUploadHackage :: HackageOptions -> IO ()
commandUploadHackage (HackageOptions path) = do
  configFile <- Toml.parseConfig path

  let projecList = Map.toList (projects configFile)
  projectAnwsers <- traverse withProject projecList

  traverse_  (upload configFile) projectAnwsers
  putStrLn "uploading complete"


upload :: ConfigFile -> ShouldUpload -> IO ()
upload configFile = \case
    (Skip name') -> putStrLn ("skipping" <> show name')
    (Upload projectName project) -> withGit (Toml.projectDir configFile) project $ do
      liftIO $ putStrLn ("uploading " <> show projectName)
      out <- runGitStdOut $ shell "cabal sdist"
      let txt = decodeUtf8 $ toStrict out
      case listToMaybe $ drop 1 $ Text.lines txt of
        Nothing -> error "no sdist output found"
        Just distOutput -> do
          exit <- runGitExitCode $ shell $ fromString $ "cabal upload --publish " <> Text.unpack distOutput
          liftIO $ putStrLn $ case exit of
            ExitFailure x -> "uploading failed for " <> (show projectName) <> " because " <> show x
            ExitSuccess -> "upload success for " <> show projectName



data ShouldUpload = Upload ProjectName Project
                  | Skip ProjectName

parseOptions :: ProjectName -> Project -> Parser ShouldUpload
parseOptions name' project = hsubparser $
  fold [
  command "y"
    (info (pure (Upload name' project))
      (progDesc $ "upload " <> show name'))
  , command "n"
    (info (pure $ Skip name')
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
