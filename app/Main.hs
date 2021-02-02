{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (id)

import Allsight.Email.Data (Alert(..), mkSubject, mkBody)
import Allsight.Notification (Notification(..), Customer(..))
import Control.Applicative ((<**>))
import Data.Aeson (withObject, (.:))
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Marshall (fromHive, allTheCustomFields)
import System.Exit (exitSuccess)

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HMap
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Short as TS
import qualified Network.HTTP.Client as Http
import qualified Options.Applicative as Options
import qualified Salesforce.Client as Sf
import qualified TheHive.CortexUtils as Hive


main :: IO ()
main = Hive.main Nothing $ \(Hive.Case input) -> do
  -- parse options
  Settings{customersFile,notificationsFile} <- Options.execParser programOptions
  -- load databases
  customers <- Json.eitherDecodeFileStrict' customersFile >>= \case
    Left err -> fail ("While decoding customers JSON: " ++ err)
    Right v -> pure (v :: [Customer])
  notifications <- Json.eitherDecodeFileStrict' notificationsFile >>= \case
    Left err -> fail ("While decoding notifications JSON: " ++ err)
    Right v -> pure (v :: [Notification])
  -- load Hive input
  config <- maybe (error "invalid config") pure $ do
    config <- fromObject =<< HMap.lookup "config" input
    endpoint <-  fromText =<< HMap.lookup "endpoint" config
    clientId <- fromText =<< HMap.lookup "clientId" config
    clientSecret <- fromText =<< HMap.lookup "clientSecret" config
    username <- fromText =<< HMap.lookup "username" config
    password <- fromText =<< HMap.lookup "password" config
    pure Sf.Config{endpoint,clientId,clientSecret,username,password}
  newCase <- either (error . ("invalid Hive input: " <>)) pure $
    flip Json.parseEither (Json.Object input)
    $ withObject "input" $ \input' -> do
      theData <- withObject "data" pure =<< input' .: "data"
      (hiveCaseId :: Int) <- theData .: "caseId"
      hiveGuiId <- theData .: "id"
      (alert, customFields) <- fromHive (Json.Object theData)
      let findCustomer Customer{id} = id == customer alert
          mCustomer = List.find findCustomer customers
          humanName = cleanRuleName notifications alert
          emailStuff = case mCustomer of
            Nothing -> []
            Just cust ->
              let subject = mkSubject cust alert humanName
                  body = mkBody cust alert notifications humanName
              in  []
                  -- [ ("EmailSubject", Json.String subject)
                  -- , ("EmailBody", Json.String $ LT.toStrict body)
                  -- ]
      pure $ Json.Object $ HMap.fromList $
            [ ("Subject", Json.String $ "Hive Case " <> T.pack (show hiveCaseId))
            , ("RecordTypeId", Json.String "0124p000000V5n5AAC")
            , ("Security_Incident_Name__c", Json.toJSON $ humanName)
            , ("Security__c", Json.toJSON $ ruleId alert)
            , ("Security_Alert_Attributes__c", "This needs to be a custom rich text format; working on it.")
            , ("Hive_Case__c", Json.toJSON $ "https://hive.noc.layer3com.com/index.html#!/case/~"<>T.pack hiveGuiId<>"/details")
            , ("Origin", "Layer 3 Alert")
            ] ++ emailStuff ++ HMap.toList (allTheCustomFields customFields)
  -- LBS.putStrLn $ Json.encode newCase -- DEBUG
  -- exitSuccess -- DEBUG
  -- talk to SalesForce
  sfManager <- Sf.newManager config
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

-- FIXME put this in `allsight-email` and de-duplicate from `allsight`
cleanRuleName :: [Notification] -> Alert -> Text
cleanRuleName notifications Alert{ruleId,ruleName} =
  case List.find (\(Notification{id}) -> id == ruleId) notifications of
    Nothing -> TS.toText ruleName
    Just Notification{friendlyName} -> friendlyName

data Settings = Settings
  { customersFile :: FilePath
  , notificationsFile :: FilePath
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
          ( Options.long "customers"
         <> Options.metavar "FILEPATH"
         <> Options.help "Path to JSON file with customer information"
          )
    <*> Options.strOption
          ( Options.long "notifications"
         <> Options.metavar "FILEPATH"
         <> Options.help "Path to JSON file with extra notification metadata"
          )
