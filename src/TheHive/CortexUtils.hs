-- a Haskell-idomatic port of https://github.com/TheHive-Project/cortexutils

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TheHive.CortexUtils
  ( main
  , Value(..)
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:))
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>))

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS

main :: Maybe FilePath -> (Value -> IO ()) -> IO ()
main jobDirOverride action = do
  jobDir <- getJobDir jobDirOverride
  input <- Json.decode @Value <$> getInput jobDir >>= \case
    Nothing -> error "json parse failed"
    Just it -> pure it
  -- TODO check tlp and pap, whatever they are
  action input
  where
  getJobDir :: Maybe FilePath -> IO FilePath
  getJobDir (Just jobDir) = pure jobDir
  getJobDir Nothing = do
    getArgs >>= \case
      (jobDir : _) -> pure jobDir
      _ -> pure "/job"
  getInput :: FilePath -> IO LBS.ByteString
  getInput jobDir = do
    let inputPath = jobDir </> "input/input.json"
    doesFileExist inputPath >>= \case
    -- new style is to read from a file in the job directory
      True -> LBS.readFile inputPath
    -- old style is to read input from standard in
      False -> LBS.getContents -- FIXME the python version has a three-second timeout before it kills itself (uses unix select to do this, but I think I can get away with haskell threads)


data Value
  = Case
    { allTheCrap :: Json.Object
    }

instance FromJSON Value where
  parseJSON = withObject "Hive.Value" $ \v ->
    v .: "dataType" >>= \case
      "thehive:case" -> pure $ Case v
      other -> fail $ "unexpected dataType: " ++ other
