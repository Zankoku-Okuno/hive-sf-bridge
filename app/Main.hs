{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (id)

import Allsight.Notification (Notification(..), Customer(..))
import Control.Applicative ((<**>))
import Control.Monad (when, forM)
import Data.Aeson (parseJSON, toJSON, withObject, (.:))
import Data.Functor ((<&>))
import System.Exit (exitSuccess)

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Elasticsearch.Client as ES
import qualified Marshall
import qualified Network.HTTP.Client as Http
import qualified Options.Applicative as Options
import qualified Salesforce.Client as Sf
import qualified TheHive.Client as Hive
import qualified TheHive.CortexUtils as Hive
import qualified TheHive.Types as Hive


main :: IO ()
main = Hive.main Nothing $ \(Hive.CaseVal hiveCase) (Hive.ResponderConfig rconfig) -> do
  -- parse options
  Settings{customersFile,notificationsFile,dryRun} <- Options.execParser programOptions
  -- load Hive input
  alert <- either (error . ("bad Hive input for alertFromHive: " <>)) pure $
    Json.parseEither Marshall.alertFromHive (Hive.theData hiveCase)
  -- load databases
  customers <- Json.eitherDecodeFileStrict' customersFile >>= \case
    Left err -> fail ("While decoding customers JSON: " ++ err)
    Right v -> pure (v :: [Customer])
  notifications <- Json.eitherDecodeFileStrict' notificationsFile >>= \case
    Left err -> fail ("While decoding notifications JSON: " ++ err)
    Right v -> pure (v :: [Notification])
  -- gather relevant data from hive and elastic
  hiveConfig <- either (error . ("invalid Hive input: " <>)) pure $
    flip Json.parseEither rconfig
    Marshall.hiveConfigFromHive
  hiveManager <- Hive.newManager hiveConfig
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
  hiveAlerts <- either (error . ("bad alerts query from Hive: " <>)) pure $
    Json.eitherDecode @[Hive.Alert] (Http.responseBody alertsResponse)
  esConfig <- either (error . ("invalid Hive input: " <>)) pure $
    flip Json.parseEither rconfig
    Marshall.esConfigFromHive
  esManager <- ES.newManager esConfig
  preEsIncidents <- forM hiveAlerts $ \Hive.Alert{sourceRef,customFields} -> do
    (cust, date) <- either (error . ("bad Hive alert: " <>)) pure $
      Json.parseEither Marshall.alertCustomFields customFields
    let esUrl = concat ["/store-", cust, "-", date, "d/_doc/", T.unpack sourceRef]
    esResponse <- ES.get esManager esUrl
    esJson <- either (error . ("bad ES incident response:" <>)) pure $
      Json.eitherDecode @Json.Value (Http.responseBody esResponse)
    pure (read cust, esJson)
  let esIncidents = snd <$> preEsIncidents
      custId = fst $ head preEsIncidents
  -- when (dryRun) $ do -- DEBUG
  --   LBS.putStrLn $ Json.encode $ toJSON esIncidents
  --   exitSuccess

  -- marshall information into salesforce-expected Json
  let newCase = Marshall.sfCaseFromHive customers hiveCase (custId, esIncidents)
  when (dryRun) $ do
    LBS.putStrLn $ Json.encode newCase
    exitSuccess
  -- talk to SalesForce
  sfConfig <- either (error . ("invalid Hive input: " <>)) pure $
    flip Json.parseEither rconfig
    Marshall.sfConfigFromHive
  sfManager <- Sf.newManager sfConfig
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
  { customersFile :: FilePath
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
    <$> Options.strOption
        (  Options.long "customers"
        <> Options.metavar "FILEPATH"
        <> Options.help "Path to JSON file with customer information"
        <> Options.action "file"
        )
    <*> Options.strOption
        (  Options.long "notifications"
        <> Options.metavar "FILEPATH"
        <> Options.help "Path to JSON file with extra notification metadata"
        <> Options.action "file"
        )
    <*> Options.switch
        (  Options.long "dry-run"
        <> Options.short 'n'
        <> Options.help "Do not create a SalesForce case, just marhall data and dump some data to stdout."
        )
