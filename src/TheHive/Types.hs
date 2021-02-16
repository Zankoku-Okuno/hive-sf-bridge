{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TheHive.Types
  ( Value(..)
  , Case(..)
  , Alert(..)
  , ResponderConfig(..)
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Text (Text)

import qualified Data.Aeson as Json
import qualified Chronos as Chronos


data Value
  = CaseVal Case

instance FromJSON Value where
  parseJSON  = withObject "Hive.Value" $ \v ->
    v .: "dataType" >>= \case
      "thehive:case" -> do
        CaseVal <$> (parseJSON =<< v .: "data")
      other -> fail $ "unexpected dataType: " ++ other

data ResponderConfig = ResponderConfig
  { allTheCrap :: Json.Value
  }

instance FromJSON ResponderConfig where
  parseJSON = withObject "Hive.ResponderConfig" $ \v -> do
    config <- v .: "config"
    pure ResponderConfig
      { allTheCrap = config
      }

data Alert = Alert
  { sourceRef :: Text
  , customFields :: Json.Value
  }

instance FromJSON Alert where
  parseJSON = withObject "Hive.Alert" $ \v -> do
    sourceRef <- v .: "sourceRef"
    customFields <- v .: "customFields"
    pure Alert
      { sourceRef
      , customFields
      }

data Case = Case
  { hiveId :: Text
  , caseNum :: Int
  , createdTime :: Chronos.Time
  , title :: Text
  , severity :: Int
  , customFields :: Json.Value
  , theData :: Json.Value -- FIXME delete
  }

instance FromJSON Case where
  parseJSON = withObject "Hive.Case" $ \v -> do
    hiveId <- v .: "id"
    caseNum <- v .: "caseId"
    createdTime <- v .: "createdAt"
    title <- v .: "title"
    severity <- v .: "severity"
    customFields <- v .: "customFields"
    pure Case
      { hiveId
      , caseNum
      , createdTime
      , title
      , severity
      , customFields
      , theData=Json.Object v
      }
