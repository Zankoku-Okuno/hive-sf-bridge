{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Marshall
  ( fromHive
  , allTheCustomFields
  ) where

import Lucid

import Allsight.Email.Data (Action(..), Alert(..))
import Control.Monad (forM_)
import Data.Aeson (withObject, (.:))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Short (ShortText)

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.HashMap.Strict as HMap
import qualified Data.List as List
import qualified Data.Text.Short as TS

-- takes only the data field of the Hive input
fromHive :: Json.Value -> Json.Parser (Alert, Json.Object)
fromHive = withObject "case data" $ \v -> do
  created <- v .: "createdAt"
  ruleName <- TS.fromText <$> v .: "title"
  ruleDescription <- v .: "description"
  customFields <- v .: "customFields" >>= withObject "custom fields" pure
  let recordsHtml = mkRecordsHtml customFields
  severity <- v .: "severity"
  let customer = 3000 -- TODO
      ruleId = 1106 -- TODO
      traceIdentifier = 0 -- TODO
  pure (Alert
    { action = Create
    , traceIdentifier
    , created
    , ruleName
    , ruleDescription
    , recordsHtml
    , customer
    , severity
    , ruleId
    }, customFields)

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
