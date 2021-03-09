{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Marshall
  ( Config(..)
  , AlertCustomFields(..)
  , sfCaseFromHive
  , SfId(..)
  , Customer(..)
  ) where

import Lucid
import Prelude hiding (id)
import qualified Prelude

import Chronos (Offset(..),SubsecondPrecision(..))
import Control.Monad (forM,forM_)
import Data.Aeson (FromJSON(..), toJSON, withObject, (.:))
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Text.Read (readMaybe)
import TheHive.CortexUtils (Case(..))

import qualified Chronos
import qualified Chronos.Locale.English as EN
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.List as List
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Short as TS
import qualified Elasticsearch.Client as ES
import qualified Salesforce.Client as SF
import qualified TheHive.Client as Hive
import qualified TheHive.CortexUtils as Hive


data Config = Config
  { hive :: Hive.Config
  , es :: ES.Config
  , sf :: SF.Config
  , allsight :: String
  }
instance FromJSON Config where
  parseJSON = withObject "Hive config" $ \v -> do
    hive <- do
      endpoint <- v .: "hive_endpoint"
      apikey <- v .: "hive_apikey"
      pure Hive.Config{endpoint,apikey}
    es <- do
      endpoint <- v .: "es_endpoint"
      username <- v .: "es_username"
      password <- v .: "es_password"
      pure ES.Config{endpoint,username,password}
    sf <- do
      endpoint <- v .: "sf_endpoint"
      clientId <- v .: "sf_client_id"
      clientSecret <- v .: "sf_client_secret"
      username <- v .: "sf_username"
      password <- v .: "sf_password"
      pure SF.Config{endpoint,clientId,clientSecret,username,password}
    allsight <- v .: "allsight_endpoint"
    pure Config{hive,allsight,es,sf}

-- data Data = Data
--   { hiveData :: Hive.Case
--   , customerData :: Customer
--   , notificationData :: Notification
--   , esData :: Json.Value
--   }

data AlertCustomFields = AlertCustomFields
  { customerId :: Int64
  , date :: String
  }
instance FromJSON AlertCustomFields where
  parseJSON = Json.withArray "Alert.customFields" $ \(toList -> fields) -> do
    adapted <- (Json.object <$>) $ forM fields $ withObject "customField" $ \v -> do
      name <- v .: "name"
      value <- v .: "value"
      pure (name :: Text, value :: Json.Value)
    flip (withObject "Alert.customFields") adapted $ \v -> do
      customerId <- (readMaybe <$> v .: "customer") >>= \case
        Nothing -> Json.parseFail "customerId not an integer"
        Just n -> pure n
      timestamp <- v .: "timestamp"
      pure AlertCustomFields
        { customerId
        , date = takeWhile (/= 'T') timestamp
        }

-- FIXME move the rule name cleaning functionality out of elastirec

sfCaseFromHive :: Customer -> Hive.Case Json.Value -> [Json.Value] -> Json.Value
sfCaseFromHive
    cust@Customer{sfAccountId}
    hiveCase@Case{caseNum,hiveId,title,severity}
    esData
  = Json.object
    [ ("Subject", toJSON $ "Hive Case " ++ show caseNum)
    , ("Hive_Case__c", toJSON hiveLink)
    , ("Origin", "Layer 3 Alert")
    , ("AccountId", toJSON sfAccountId)
    , ("RecordTypeId", toJSON @Text "0124p000000V5n5AAC")
    , ("Security_Incident_Name__c", toJSON title)
    , ("Security__c", toJSON ruleId) -- TODO do we still want to use this id?
    , ("Security_Incident_Severity__c", toJSON severity)
    -- TODO: email subject?
    , ("Security_Alert_Attributes__c", toJSON $
        mkBody cust hiveCase esData title
      )
    , ("Security_Incident_UUID__c", toJSON traceId)
    , ("Description", toJSON $ aggregateDescription esData)
    ]
  where
  hiveLink = "https://hive.noc.layer3com.com/index.html#!/case/~" <> hiveId <> "/details"
  ruleId = delve (head esData) ["_source", "incident", "id"]
  traceId = delve (head esData) ["_source", "trace", "id"]


aggregateDescription :: [Json.Value] -> Text
aggregateDescription esData = T.intercalate "\n\n" $ List.nub . catMaybes $
  flip map esData $ \esDatum ->
    delve esDatum ["_source", "incident", "description"] >>= \case
      Json.String str -> Just str
      _ -> Nothing

mkBody :: Customer -> Hive.Case Json.Value -> [Json.Value] -> Text -> TL.Text
mkBody
    Customer{name}
    hiveCase
    esData
    humanName = Lucid.renderText $ do
  h2_ $ toHtml $
    name <> " - Security Incident Report"
  p_ $ toHtml $
    Chronos.encode_YmdIMS_p EN.upper (SubsecondPrecisionFixed 0) Chronos.hyphen
      (Chronos.offsetDatetimeDatetime
        (Chronos.timeToOffsetDatetime (Offset (-300)) -- FIXME: customer offset
        (Hive.createdTime hiveCase)))
  p_ $ h2_ $ toHtml humanName
  p_ $ toHtml $ aggregateDescription esData
  -- p_ $ hyperlinkToTraceId traceIdentifier
  -- case kibanaIndexPattern of
  --   0 -> p_ $ hyperlinkToRelevant allEventsUuid created traceIdentifier
  --   pat -> p_ $ hyperlinkToRelevant pat created traceIdentifier
  h3_ [] "Attributes:"
  forM_ esData $ \es -> do
    details_ [] $ do
      summary_ [] $ h4_ [] $ do
        maybeM_ (delve es ["_source", "trace", "id"]) $ \case
          Json.String str -> "Incident ID " <> toHtml str
          _ -> "Incident ID not known"
      dl_ [] $ do
        esAttr es ["destination", "addresses"]
        esAttr es ["destination", "ip"]
        esAttr es ["destination", "ips"]
        esAttr es ["destination", "ips_count"]
        esAttr es ["destination", "port"]
        esAttr es ["destination", "ports"]
        esAttr es ["destination", "zones"]
        esAttr es ["event", "action"]
        esAttr es ["event", "category"]
        esAttr es ["event", "created"]
        esAttr es ["event", "dataset"]
        esAttr es ["event", "end"]
        esAttr es ["event", "id"]
        esAttr es ["event", "kind"]
        esAttr es ["event", "module"]
        esAttr es ["event", "severity"]
        esAttr es ["event", "start"]
        esAttr es ["file", "md5s"]
        esAttr es ["host", "names"]
        esAttr es ["incident", "category"]
        esAttr es ["incident", "description"]
        esAttr es ["incident", "id"]
        esAttr es ["incident", "name"]
        esAttr es ["incident", "severity"]
        esAttr es ["mitre-attack-technique"]
        esAttr es ["network", "application"]
        esAttr es ["network", "applications"]
        esAttr es ["network", "direction"]
        esAttr es ["network", "directions"]
        esAttr es ["observer", "name"]
        esAttr es ["observer", "product"]
        esAttr es ["observer", "type"]
        esAttr es ["observer", "vendor"]
        esAttr es ["provenance", "event", "dataset"]
        esAttr es ["provenance", "event", "module"]
        esAttr es ["provenance", "observer", "product"]
        esAttr es ["provenance", "observer", "vendor"]
        esAttr es ["source", "addresses"]
        esAttr es ["source", "ip"]
        esAttr es ["source", "ips"]
        esAttr es ["source", "ips_count"]
        esAttr es ["source", "port"]
        esAttr es ["source", "ports"]
        esAttr es ["source", "user", "names"]
        esAttr es ["source", "user", "names_count"]
        esAttr es ["source", "zones"]
        esAttr es ["trace", "id"]
        esAttr es ["url", "domains"]
        esAttr es ["url", "domains_count"]

  -- case List.find (\(Notification{id}) -> id == ruleId) notifications of
  --   Nothing -> pure ()
  --   Just Notification{playbooks,suggestedActions,referenceLinks} -> p_ $ do
  --     when (PM.sizeofArray playbooks /= 0) $ do
  --       h3_ "Playbooks:"
  --       forM_ playbooks $ \playbook -> do
  --         p_ $ toHtml playbook
  --     when (PM.sizeofArray suggestedActions /= 0) $ do
  --       h3_ "Suggested Actions:"
  --       forM_ suggestedActions $ \suggestedAction -> do
  --         p_ $ toHtml suggestedAction
  --     when (PM.sizeofArray referenceLinks /= 0) $ do
  --       h3_ "References:"
  --       forM_ referenceLinks $ \referenceLink -> do
  --         p_ $ toHtml referenceLink
  where
  esAttr es path = do
    maybeM_ (delve es ("_source":path)) $ jsonAsHtml (T.intercalate "." path)



instance Json.FromJSON ShortText where
  parseJSON = (TS.fromText <$>) . Json.parseJSON


newtype SfId = SfId Text
instance Json.FromJSON SfId where
  parseJSON = withObject "salesforce response" $ \v -> do
    SfId <$> v .: "id"


maybeM_ :: (Monad m) => Maybe a -> (a -> m b) -> m ()
maybeM_ Nothing _ = pure ()
maybeM_ (Just a) f = f a >> pure ()

jsonAsHtml :: Text -> Json.Value -> Html ()
jsonAsHtml label = helper withLabel
  where
  helper :: (Html () -> Html ()) -> Json.Value -> Html ()
  helper wrap = \case
    Json.String str -> wrap $ toHtml str
    Json.Number 0 -> pure ()
    Json.Number n -> case Sci.floatingOrInteger n of
      Right i -> wrap $ toHtml $ show @Int i
      Left r -> wrap $ toHtml $ show @Double r
    Json.Array (toList -> [x]) -> helper wrap x
    Json.Array xs -> wrap $ ul_ [] $ forM_ xs $ \x -> li_ [] $ helper Prelude.id x
    it -> error $ "can't render json: " ++ show it
  withLabel :: Html () -> Html ()
  withLabel content = do
    dt_ [] $ toHtml label
    dd_ [] content

delve :: Json.Value -> [Text] -> Maybe Json.Value
delve v0 path0 =Json.parseMaybe (loop path0) v0
  where
  loop [] = pure
  loop (prop:path) = withObject "" $ (loop path =<<) . (.: prop)


------

data Customer = Customer
  { id :: !Int64
  , sfAccountId :: !Text
  , name :: !Text
  -- , email :: !Text
  -- , severity :: !Int64
  -- , contacts :: !Contacts
  -- , kibanaIndexPattern :: {-# UNPACK #-} !Word128
  }

instance FromJSON Customer where
  parseJSON = Json.withObject "Customer" $ \v -> do
    id <- v .: "id"
    sfAccountId <- v .: "salesforce_account_id"
    name <- v .: "name"
    pure Customer{id,sfAccountId,name}
    -- email <- v .: "email"
    -- severity <- v .: "severity"
    -- contacts <- v .: "contacts"
    -- kibanaIndexPattern' <- v .: "kibana_index_pattern"
    -- case UUID.decodeHyphenated (Bytes.fromShortByteString (TS.toShortByteString (TS.fromText kibanaIndexPattern'))) of
    --   Nothing -> fail "kibana_index_pattern should be a UUID"
    --   Just kibanaIndexPattern -> pure Customer
    --     { id , name , email , severity , contacts , kibanaIndexPattern }
