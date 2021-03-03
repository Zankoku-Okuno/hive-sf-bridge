{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (id)

import Allsight.Notification (Notification(..))
import Control.Applicative ((<**>), optional)
import Control.Monad (when, forM)
import Data.Aeson (toJSON)
import Data.Functor ((<&>))
import Marshall (Customer(..))
import System.Exit (exitSuccess)

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as List
import qualified Data.Text as T
import qualified Elasticsearch.Client as ES
import qualified Marshall
import qualified Network.HTTP.Client as Http
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
  Settings{jobDir,customersFile,notificationsFile,dryRun} <- Options.execParser programOptions
  Hive.main jobDir $ \(Hive.CaseVal hiveCase) (Hive.ResponderConfig rconfig) -> do
    -- parse configuration from TheHive
    conf <- fromJsonOrDie @Marshall.Config "bad config from TheHive" rconfig
    -- load databases
    customers <- Json.eitherDecodeFileStrict' customersFile >>= \case
      Left err -> fail ("While decoding customers JSON: " ++ err)
      Right v -> pure (v :: [Customer])
    -- notifications <- Json.eitherDecodeFileStrict' notificationsFile >>= \case
    --   Left err -> fail ("While decoding notifications JSON: " ++ err)
    --   Right v -> pure (v :: [Notification])

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
      LBS.putStrLn $ Json.encode newCase
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
    pure . Right $ "Salesforce case ID is: " <> sfId


data Settings = Settings
  { jobDir :: Maybe FilePath
  , customersFile :: FilePath
  , notificationsFile :: FilePath
  , dryRun :: Bool
  }

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
    <*> Options.strOption
        (  Options.long "customers"
        <> Options.metavar "FILEPATH"
        <> Options.value "/etc/hive-sf-bridge/customers.json"
        <> Options.help "Path to JSON file with customer information"
        <> Options.action "file"
        )
    <*> Options.strOption
        (  Options.long "notifications"
        <> Options.metavar "FILEPATH"
        <> Options.value "/etc/hive-sf-bridge/notifications.json"
        <> Options.help "Path to JSON file with extra notification metadata"
        <> Options.action "file"
        )
    <*> Options.switch
        (  Options.long "dry-run"
        <> Options.short 'n'
        <> Options.help "Do not create a SalesForce case, just marhall data and dump some data to stdout."
        )
