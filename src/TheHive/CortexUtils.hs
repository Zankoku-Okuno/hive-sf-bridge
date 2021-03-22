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
  , Response(..)
  , Operation(..)
  ) where

import Control.Exception (SomeException,catch)
import Data.Aeson (FromJSON,ToJSON(toJSON), (.=))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import TheHive.Types (Value(..), ResponderConfig, Case(..))

import qualified Data.Text as T
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS

data Response
  = SuccessResponse
    { message :: Text
    , operations :: [Operation]
    }
  -- TODO FailureResponse

data Operation
  = AddTagToCase Text
  -- TODO more operations available at https://github.com/TheHive-Project/CortexDocs/blob/7e366670c727ad57042174c180033856ef1dc6ba/api/how-to-create-a-responder.md#output

type Responder customFields
  =  Value customFields
  -> ResponderConfig
  -> IO (Either Text Response)

-- `jobDir` is expected to be drawn from an optional string argument at the start of the command line options
main :: forall customFields. (FromJSON customFields) =>
  Maybe FilePath -> Responder customFields -> IO ()
main jobDirOverride action = do
  jobDir <- getJobDir
  go jobDir `catch` showAllExns >>= \case
    Right result -> setOutput jobDir $ toJSON result
    Left errmsg -> do
      -- setOutput jobDir $ Json.object
      setOutput Nothing $ Json.object -- DEBUG
        [ "success" .= Json.toJSON False
        , "message" .= Json.toJSON errmsg
        ]
      exitFailure
  where
  go jobDir = do
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
    action value config
  getJobDir :: IO (Maybe FilePath)
  getJobDir = do
    let jobDir = fromMaybe "/job" jobDirOverride
    let inputPath = jobDir </> "input/input.json"
    doesFileExist inputPath <&> \case
      True -> Just jobDir
      False -> Nothing
  getInput :: Maybe FilePath -> IO LBS.ByteString
  -- new style is to read from a file in the job directory
  getInput (Just jobDir) = LBS.readFile (jobDir </> "input/input.json")
  -- old style is to read input from standard in
  getInput Nothing = LBS.getContents -- FIXME the python version has a three-second timeout before it kills itself (uses unix select to do this, but I think I can get away with haskell threads)
  setOutput :: Maybe FilePath -> Json.Value -> IO ()
  setOutput (Just jobDir) out = do
    createDirectoryIfMissing True (jobDir </> "output")
    LBS.writeFile (jobDir </> "output" </> "output.json") (Json.encode out)
  setOutput Nothing out = LBS.putStr (Json.encode out)
  showAllExns :: SomeException -> IO (Either Text a)
  showAllExns = pure . Left . T.pack . show


instance ToJSON Response where
  toJSON SuccessResponse{message,operations} = Json.object
    [ "success" .= toJSON True
    , "full" .= Json.object [ "message" .= toJSON message ]
    , "operations" .= toJSON operations
    ]

instance ToJSON Operation where
  toJSON (AddTagToCase tag) = Json.object
    [ "type" .= ("AddTagToCase" :: Json.Value)
    , "tag" .= toJSON tag
    ]
