{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (id)

import Control.Applicative ((<**>), optional)
import Control.Monad (when, forM)
import Data.Aeson (toJSON)
import Data.Functor ((<&>))
import TheHive.CortexUtils (Operation(..))
import Marshall (Customer(..), allsight)
import System.Exit (exitSuccess)

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar
import qualified Data.List as List
import qualified Data.Text as T
import qualified Elasticsearch.Client as ES
import qualified Marshall
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Options.Applicative as Options
import qualified Salesforce.Client as Sf
import qualified TheHive.Client as Hive
import qualified TheHive.CortexUtils as Hive
import qualified TheHive.Types as Hive

fromJsonOrDie :: forall a. (Json.FromJSON a) => String -> Json.Value -> IO a
fromJsonOrDie errPrefix inp =
  either (error . ((errPrefix ++ ": ") ++)) pure $
    Json.parseEither Json.parseJSON inp
decodeJsonOrDie :: forall a. (Json.FromJSON a) => String -> LBS.ByteString -> IO a
decodeJsonOrDie errPrefix inp =
  either (\x -> error $ concat [errPrefix, ": ", x]) pure $
    Json.eitherDecode inp

main :: IO ()
main = do
  Settings{jobDir,dryRun} <- Options.execParser programOptions
  Hive.main jobDir $ \(Hive.CaseVal hiveCase) (Hive.ResponderConfig rconfig) -> do
    -- parse configuration from TheHive
    conf <- fromJsonOrDie @Marshall.Config "bad config from TheHive" rconfig
    -- load customer database
    (customers :: [Customer]) <- do
      manager <- Http.newManager Http.tlsManagerSettings
      request <- Http.parseRequest (concat ["https://", allsight conf, "/customers.json"])
      response <- Http.httpLbs request manager
      pure $ case Json.eitherDecodeStrict' (LBS.toStrict $ Http.responseBody response) of
        Right v -> v
        Left err -> error ("While decoding customers JSON: " ++ err)

    -- accumulate alert and incident data
    hiveManager <- Hive.newManager (Marshall.hive conf)
    let alertsUrl = "/api/v1/query?name=get-case-alerts" ++ T.unpack (Hive.hiveId hiveCase)
    alertsResponse <- Hive.post hiveManager alertsUrl $ Json.object
      [ ("query", toJSON
          [ Json.object
            [ ("_name", "getCase")
            , ("idOrName", toJSON $ Hive.hiveId hiveCase)
            ]
          , Json.object
            [ ("_name", "alerts")
            ]
          ]
        )
      ]
    hiveAlerts <- decodeJsonOrDie @[Hive.Alert Marshall.AlertCustomFields]
      "bad alerts from TheHive" (Http.responseBody alertsResponse)
    customer <- case hiveAlerts of
      [] -> error "no alerts associated with case"
      (Hive.Alert{customFields=Marshall.AlertCustomFields{customerId}} : _) -> do
        case List.find (\Customer{id} -> id == customerId) customers of
          Nothing -> error "responder does not recognize Hive case's customer id"
          Just c -> pure c
    let Customer{id=customerId} = customer
    esManager <- ES.newManager (Marshall.es conf)
    esIncidents <- forM hiveAlerts $ \Hive.Alert{sourceRef,customFields} -> do
      let date = Marshall.date customFields
      let esUrl = concat ["/store-", show customerId, "-", date, "d/_doc/", T.unpack sourceRef]
      esResponse <- ES.get esManager esUrl
      esJson <- decodeJsonOrDie @Json.Value
        "bad ES incident response" (Http.responseBody esResponse)
      pure esJson
    -- when (dryRun) $ do -- DEBUG
    --   LBS.putStrLn $ Json.encode $ toJSON esIncidents
    --   exitSuccess

    -- marshall information into salesforce-expected Json
    let newCase = Marshall.sfCaseFromHive customer hiveCase esIncidents
    when (dryRun) $ do
      LBSChar.putStrLn $ Json.encode newCase
      exitSuccess

    -- talk to SalesForce
    sfManager <- Sf.newManager (Marshall.sf conf)
                  <&> \x -> x{Sf.prettyPrint=True} -- DEBUG
    response <- Sf.post sfManager "sobjects/Case" newCase
    let theError = "unexpected salesforce reply:\n" <> (show $ Http.responseBody response)
    Marshall.SfId sfId <- either (error theError) pure $
      Json.eitherDecode (Http.responseBody response)
    -- maybe (error theError) pure $ do
    --   obj <- Json.decode @Json.Object (Http.responseBody response)
    --   HMap.lookup "id" obj >>= fromText

    let sfLink = "https://layer3.lightning.force.com/lightning/r/Case/" <> sfId <> "/view"
    pure . Right $ Hive.SuccessResponse
      { message = "Salesforce case ID is: " <> sfId
      , operations =
          [ AddTagToCase "sfbridge"
          , AddCustomField "salesforce-link" sfLink
          ]
      }


data Settings = Settings
  { jobDir :: Maybe FilePath
  , dryRun :: Bool
  }
  deriving(Show)

programOptions :: Options.ParserInfo Settings
programOptions = Options.info (parser <**> Options.helper)
   ( Options.fullDesc
  <> Options.progDesc "Send cases from The Hive to Salseforce"
  <> Options.header "hive-sf-bridge"
   )
  where
  parser :: Options.Parser Settings
  parser = Settings
    <$> optional (Options.strArgument
        (  Options.metavar "JOBDIR"
        <> Options.help "Path for Cortex to specify a directory for input/output files."
        <> Options.action "directory"
        ))
    <*> Options.switch
        (  Options.long "dry-run"
        <> Options.short 'n'
        <> Options.help "Do not create a SalesForce case, just marhall data and dump some data to stdout."
        )
