{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Marshall
  ( alertFromHive
  , alertCustomFields
  , allTheCustomFields
  , esConfigFromHive
  , hiveConfigFromHive
  , sfConfigFromHive
  , sfCaseFromHive
  , SfId(..)
  ) where

import Lucid
import Prelude hiding (id)

import Allsight.Email.Data (Action(..), Alert(..))
import Allsight.Notification (Notification(..), Customer(..))
import Control.Monad (forM,forM_)
import Data.Aeson (toJSON, withObject, (.:))
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import TheHive.CortexUtils (Case(..))

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.HashMap.Strict as HMap
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Short as TS
import qualified Elasticsearch.Client as ES
import qualified RichText
import qualified Salesforce.Client as SF
import qualified TheHive.Client as Hive
import qualified TheHive.CortexUtils as Hive

alertCustomFields :: Json.Value -> Json.Parser (String, String)
alertCustomFields customFields = do
  fields <- Json.parseJSON @[Json.Value] customFields
  adapted <- (Json.object <$>) $ forM fields $ withObject "customField" $ \v -> do
    name <- v .: "name"
    value <- v .: "value"
    pure (name :: Text, value :: Json.Value)
  flip (withObject "customFields") adapted $ \v -> do
    cust <- v .: "customer"
    timestamp <- v .: "timestamp"
    pure (cust, takeWhile (/= 'T') timestamp)

sfConfigFromHive :: Json.Value -> Json.Parser SF.Config
sfConfigFromHive = withObject "config" $ \config -> do
  endpoint <-  config .: "sf_endpoint"
  clientId <- config .: "sf_clientId"
  clientSecret <- config .: "sf_clientSecret"
  username <- config .: "sf_username"
  password <- config .: "sf_password"
  pure SF.Config{endpoint,clientId,clientSecret,username,password}

esConfigFromHive :: Json.Value -> Json.Parser ES.Config
esConfigFromHive = withObject "config" $ \config -> do
  endpoint <- config .: "es_endpoint"
  username <- config .: "es_username"
  password <- config .: "es_password"
  pure ES.Config{endpoint,username,password}

hiveConfigFromHive :: Json.Value -> Json.Parser Hive.Config
hiveConfigFromHive = withObject "config" $ \config -> do
  endpoint <- config .: "hive_endpoint"
  apikey <- config .: "hive_apikey"
  pure Hive.Config{endpoint,apikey}


-- takes only the data field of the Hive input
alertFromHive :: Json.Value -> Json.Parser Alert
alertFromHive = withObject "case data" $ \v -> do
  created <- v .: "createdAt"
  ruleName <- TS.fromText <$> v .: "title"
  ruleDescription <- v .: "description"
  customFields <- v .: "customFields" >>= withObject "custom fields" pure
  let recordsHtml = mkRecordsHtml customFields
  severity <- v .: "severity"
  let customer = 3000 -- TODO
      ruleId = 1106 -- TODO
      traceIdentifier = 0 -- TODO
  pure Alert
    { action = Create
    , traceIdentifier
    , created
    , ruleName
    , ruleDescription
    , recordsHtml
    , customer
    , severity
    , ruleId
    }

-- FIXME move the rule name cleaning functionality out of elastirec

mkRecordsHtml :: Json.Object -> Html ()
mkRecordsHtml obj = forM_ (List.sort $ HMap.keys obj) $ \k -> do
  case HMap.lookup k obj of
    Just (Json.Number 0) -> pure ()
    Just (Json.Number n) -> do
      strong_ $ toHtml k >> ": "
      toHtml $ show n
      br_ []
    Just (Json.String str) -> do
      strong_ $ toHtml k >> ": "
      toHtml str
      br_ []
    _ -> pure ()

sfCaseFromHive :: [Customer] -> Hive.Case -> (Int64, [Json.Value]) -> Json.Value
sfCaseFromHive
    customers
    (hiveCase@Case{caseNum,hiveId})
    (custId, esData)
  = Json.object
  $ standardStuff ++ emailStuff
  where
  humanName = "TODO some rule name" -- TODO cleanRuleName notifications alert
  standardStuff =
    [ ("Subject", toJSON $ "Hive Case " ++ show caseNum)
    , ("Hive_Case__c", toJSON hiveLink)
    , ("Origin", "Layer 3 Alert")
    , ("RecordTypeId", Json.String "0124p000000V5n5AAC")
    -- , ("Security_Incident_Name__c", toJSON $ humanName) -- TODO
    -- , ("Security__c", toJSON $ ruleId alert) -- TODO
    -- TODO: email field
    ]
  emailStuff = case List.find (\Customer{id} -> id == custId) customers of
    Nothing -> []
    Just cust ->
      [ ("Security_Alert_Attributes__c", toJSON $ RichText.mkBody cust hiveCase esData humanName)
      ]
  hiveLink = "https://hive.noc.layer3com.com/index.html#!/case/~" <> hiveId <> "/details"

sfCaseFromHiveOld :: Alert -> ([Customer], [Notification]) -> Json.Value -> Json.Parser Json.Value
sfCaseFromHiveOld alert (customers, notifications) = withObject "data" $ \theData -> do
  (hiveCaseId :: Int) <- theData .: "caseId"
  hiveGuiId <- theData .: "id"
  customFields <- theData .: "customFields"
  let humanName = cleanRuleName notifications alert
      emailStuff = case List.find (\Customer{id} -> id == customer alert) customers of
        Nothing -> []
        Just cust ->
          [ ("Security_Alert_Attributes__c", toJSON $ RichText.mkBodyOld cust alert notifications humanName)
          ]
  pure $ Json.object $
    [ ("Subject", toJSON $ "Hive Case " ++ show hiveCaseId)
    , ("RecordTypeId", Json.String "0124p000000V5n5AAC")
    , ("Security_Incident_Name__c", toJSON $ humanName)
    , ("Security__c", toJSON $ ruleId alert)
    , ("Hive_Case__c", toJSON $ "https://hive.noc.layer3com.com/index.html#!/case/~"<>T.pack hiveGuiId<>"/details")
    , ("Origin", "Layer 3 Alert")
    ]
    ++ HMap.toList (allTheCustomFields customFields)
    ++ emailStuff

-- input `customFields` from Hive, create fields for layer3 salesforce cases
allTheCustomFields :: Json.Object -> Json.Object
allTheCustomFields customFields = HMap.fromList $ catMaybes $ oneField <$>
  [ ("incident.description", "Description")
  , ("incident.severity", "Security_Incident_Severity__c")
  , ("trace.id", "Security_Incident_UUID__c")
  ]
  where
  oneField :: (Text, Text) -> Maybe (Text, Json.Value)
  oneField (hive, sf) = (sf,) <$> Json.parseMaybe (accessor hive) customFields
  accessor :: Text -> Json.Object -> Json.Parser Json.Value
  accessor field obj = obj .: field >>= withObject "custom field" (.: "string")
  -- accessor (:fields) = withObject "" (\obj -> accessor fields =<< obj .: field)

instance Json.FromJSON ShortText where
  parseJSON = (TS.fromText <$>) . Json.parseJSON

-- FIXME put this in `allsight-email` and de-duplicate from `allsight`
cleanRuleName :: [Notification] -> Alert -> Text
cleanRuleName notifications Alert{ruleId,ruleName} =
  case List.find (\(Notification{id}) -> id == ruleId) notifications of
    Nothing -> TS.toText ruleName
    Just Notification{friendlyName} -> friendlyName


newtype SfId = SfId Text
instance Json.FromJSON SfId where
  parseJSON = withObject "salesforce response" $ \v -> do
    v .: "id"
