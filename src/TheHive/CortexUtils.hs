{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- a Haskell-idomatic port of https://github.com/TheHive-Project/cortexutils
module TheHive.CortexUtils
  ( Responder
  , main
  , Value(..)
  , Case(..)
  ) where

import Control.Exception (SomeException,catch)
import Data.Aeson (FromJSON)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import TheHive.Types (Value(..), ResponderConfig, Case(..))


import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS

type Responder customFields
  =  Value customFields
  -> ResponderConfig
  -> IO (Either Text Text)

-- `jobDir` is expected to be drawn from an optional string argument at the start of the command line options
main :: forall customFields. (FromJSON customFields) =>
  Maybe FilePath -> Responder customFields -> IO ()
main jobDirOverride action = do
  jobDir <- getJobDir
  input <- getInput jobDir
  let sourceDescr = case jobDir of
        Nothing -> "stdin"
        Just path -> path
  value <- case Json.eitherDecode @(Value customFields) input of
    Left err -> error $ "bad responder input [" ++ sourceDescr ++ "]: " ++ err
    Right it -> pure it
  config <- case Json.eitherDecode @ResponderConfig input of
    Left err -> error $ "bad responder config [" ++ sourceDescr ++ "]: " ++ err
    Right it -> pure it
  -- TODO check tlp and pap, whatever they are
  action value config `catch` showAllExns >>= \case
    Right msg -> setOutput jobDir $ HMap.fromList $
      [ ("success", Json.toJSON True)
      , ("full", Json.Object $ HMap.fromList
          [ ("message", Json.toJSON msg)
          ] -- TODO
        )
      , ("operations", Json.toJSON @[Json.Value] []) -- TODO
      ]
    Left errmsg -> do
      setOutput jobDir $ HMap.fromList $
        [ ("success", Json.toJSON False)
        , ("message", Json.toJSON errmsg)
        ]
      exitFailure
  where
  getJobDir :: IO (Maybe FilePath)
  getJobDir = do
    let jobDir = fromMaybe "/job" jobDirOverride
    let inputPath = jobDir </> "input/input.json"
    doesFileExist inputPath <&> \case
      True -> Just inputPath
      False -> Nothing
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
