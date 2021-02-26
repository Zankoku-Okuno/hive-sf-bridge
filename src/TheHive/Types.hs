{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
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


data Value caseCustom
  = CaseVal (Case caseCustom)

instance (FromJSON caseCustom) => FromJSON (Value caseCustom) where
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

data Alert custom = Alert
  { sourceRef :: Text
  , customFields :: custom
  }

instance (FromJSON custom) => FromJSON (Alert custom) where
  parseJSON = withObject "Hive.Alert" $ \v -> do
    sourceRef <- v .: "sourceRef"
    customFields <- v .: "customFields"
    pure Alert
      { sourceRef
      , customFields
      }

data Case custom = Case
  { hiveId :: Text
  , caseNum :: Int
  , createdTime :: Chronos.Time
  , title :: Text
  , severity :: Int
  , customFields :: custom
  }

instance (FromJSON custom) => FromJSON (Case custom) where
  parseJSON = withObject "Hive.Case" $ \v -> do
    hiveId <- v .: "id"
    caseNum <- v .: "caseId"
    hiveTime <- v .: "createdAt"
    let createdTime = Chronos.Time (hiveTime * 1_000_000) -- WARNING apparently I get number of milliseconds after epoch?
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
      }
