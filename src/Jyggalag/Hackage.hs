-- | Upload to hackage
--   this attempts to upload, failures are expected.
--   if a version isn't bumped this will just fail.
--   if you made a mistake, just bump minor version and re-upload.
module Jyggalag.Hackage
  ( commandUploadHackage
  , HackageOptions(..)
  )
where

import qualified Jyggalag.Toml as Toml
import qualified Data.Map as Map
import Jyggalag.Toml
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
  traverse_  (upload configFile) $ withProject <$> projecList
  putStrLn "uploading complete"


upload :: ConfigFile -> ShouldUpload -> IO ()
upload configFile = \case
    (NotSet name') -> putStrLn $ "ignoring " <> show name' <> " upload because upload wasn't set"
    (Skip name') -> putStrLn $ "ignoring upload for " <> show name' <> " because  upload was set to false"
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
                  | NotSet ProjectName

withProject :: (ProjectName, Project) -> ShouldUpload
withProject (name, project) =
  case (release project) of
    Nothing -> NotSet name
    Just False -> Skip name
    Just True -> Upload name project
