-- a Haskell-idomatic port of https://github.com/TheHive-Project/cortexutils

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TheHive.CortexUtils
  ( main
  , Value(..)
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Functor ((<&>))
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Data.Text (Text)
import Control.Exception (SomeException,catch)

import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS

main :: Maybe FilePath -> (Value -> IO (Either Text Text)) -> IO ()
main jobDirOverride action = do
  jobDir <- getJobDir
  input <- Json.decode @Value <$> getInput jobDir >>= \case
    Nothing -> error "json parse failed"
    Just it -> pure it
  -- TODO check tlp and pap, whatever they are
  action input `catch` showAllExns >>= \case
    Right msg -> setOutput jobDir $ HMap.fromList $
      [ ("success", Json.toJSON True)
      , ("full", Json.Object $ HMap.fromList
          [ ("message", Json.toJSON msg)
          ] -- TODO
        )
      , ("operations", Json.toJSON @[Json.Value] []) -- TODO
      ]
    Left errmsg -> setOutput jobDir $ HMap.fromList $
      [ ("success", Json.toJSON False)
      , ("message", Json.toJSON errmsg)
      ]
  where
  getJobDir :: IO (Maybe FilePath)
  getJobDir = do
    jobDir <- maybe fromArgs pure jobDirOverride
    let inputPath = jobDir </> "input/input.json"
    doesFileExist inputPath <&> \case
      True -> Just inputPath
      False -> Nothing
    where
    fromArgs = getArgs >>= \case { (jobDir:_) -> pure jobDir; _ -> pure "/job" }
  getInput :: Maybe FilePath -> IO LBS.ByteString
  -- new style is to read from a file in the job directory
  getInput (Just jobDir) = LBS.readFile (jobDir </> "input/input.json")
  -- old style is to read input from standard in
  getInput Nothing = LBS.getContents -- FIXME the python version has a three-second timeout before it kills itself (uses unix select to do this, but I think I can get away with haskell threads)
  setOutput :: Maybe FilePath -> Json.Object -> IO ()
  setOutput (Just jobDir) out = do
    createDirectoryIfMissing True (jobDir </> "output")
    LBS.writeFile (jobDir </> "output/output.json") (Json.encode out)
  setOutput Nothing out = LBS.putStr (Json.encode out)
  showAllExns :: SomeException -> IO (Either Text a)
  showAllExns = pure . Left . T.pack . show 


data Value
  = Case
    { allTheCrap :: Json.Object
    }

instance FromJSON Value where
  parseJSON = withObject "Hive.Value" $ \v ->
    v .: "dataType" >>= \case
      "thehive:case" -> pure $ Case v
      other -> fail $ "unexpected dataType: " ++ other
