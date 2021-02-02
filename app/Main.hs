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
import Control.Monad (when)
import Data.Functor ((<&>))
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import System.Exit (exitSuccess)

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HMap
import qualified Marshall
import qualified Network.HTTP.Client as Http
import qualified Options.Applicative as Options
import qualified Salesforce.Client as Sf
import qualified TheHive.CortexUtils as Hive


main :: IO ()
main = Hive.main Nothing $ \(Hive.Case{config=hiveConfig,payload}) -> do
  -- parse options
  Settings{customersFile,notificationsFile,dryRun} <- Options.execParser programOptions
  -- load databases
  customers <- Json.eitherDecodeFileStrict' customersFile >>= \case
    Left err -> fail ("While decoding customers JSON: " ++ err)
    Right v -> pure (v :: [Customer])
  notifications <- Json.eitherDecodeFileStrict' notificationsFile >>= \case
    Left err -> fail ("While decoding notifications JSON: " ++ err)
    Right v -> pure (v :: [Notification])
  -- load Hive input
  alert <- either (error . ("bad Hive input for alertFromHive: " <>)) pure $
    Json.parseEither Marshall.alertFromHive payload
  newCase <- either (error . ("bad Hive input for sfCaseFromHive: " <>)) pure $
    flip Json.parseEither payload
    (Marshall.sfCaseFromHive alert (customers, notifications))
  when (dryRun) $ do
    LBS.putStrLn $ Json.encode newCase
    exitSuccess
  -- talk to SalesForce
  sfConfig <- either (error . ("invalid Hive input: " <>)) pure $
    flip Json.parseEither hiveConfig
    Marshall.sfConfigFromHive
  sfManager <- Sf.newManager sfConfig
                <&> \x -> x{Sf.prettyPrint=True} -- DEBUG
  response <- Sf.post sfManager "sobjects/Case" newCase
  let theError = "unexpected salesforce reply:\n" <> (show $ Http.responseBody response)
  sfId <- maybe (error theError) pure $ do
    obj <- Json.decode @Json.Object (Http.responseBody response)
    HMap.lookup "id" obj >>= fromText
  pure . Right $ "Salesforce case ID is: " <> sfId

fromObject :: Json.Value -> Maybe (HMap.HashMap Text Json.Value)
fromObject (Json.Object obj) = Just obj
fromObject _ = Nothing

fromText :: Json.Value -> Maybe Text
fromText (Json.String str) = Just str
fromText _ = Nothing

fromInt :: Json.Value -> Maybe Int
fromInt (Json.Number n) = toBoundedInteger n
fromInt _ = Nothing

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
        )
    <*> Options.strOption
        (  Options.long "notifications"
        <> Options.metavar "FILEPATH"
        <> Options.help "Path to JSON file with extra notification metadata"
        )
    <*> Options.switch
        (  Options.long "dry-run"
        <> Options.short 'n'
        <> Options.help "Do not create a SalesForce case, just marhall data and dump some data to stdout."
        )
