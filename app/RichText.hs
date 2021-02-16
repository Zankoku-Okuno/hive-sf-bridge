{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RichText
  ( mkBody
  , mkBodyOld -- TODO delete me
  ) where

import Prelude hiding (id)
import Lucid

import Allsight.Email.Data (Alert(..),hyperlinkToRelevant,hyperlinkToTraceId)
import Allsight.Notification (allEventsUuid)
import Allsight.Notification (Notification(..),Customer(..))
import Chronos (Offset(..),SubsecondPrecision(..))
import Control.Monad (when, forM_)
import Data.Aeson (withObject, (.:))
import Data.Text (Text)

import qualified Chronos
import qualified Chronos.Locale.English as EN
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.List as List
import qualified Data.Primitive as PM
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified TheHive.Types as Hive

mkBody :: Customer -> Hive.Case -> [Json.Value] -> Text -> TL.Text
mkBody
    Customer{name,kibanaIndexPattern}
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
  -- p_ $ toHtml ruleDescription
  -- p_ $ hyperlinkToTraceId traceIdentifier
  -- case kibanaIndexPattern of
  --   0 -> p_ $ hyperlinkToRelevant allEventsUuid created traceIdentifier
  --   pat -> p_ $ hyperlinkToRelevant pat created traceIdentifier
  h4_ [] "Attributes:"
  forM_ esData $ \es -> do
    details_ [] $ do
      summary_ [] $ h5_ [] $ do
        maybeM_ (delve es ["_source", "trace", "id"]) $ \case
          Json.String str -> "Incident ID " <> toHtml str
          _ -> "Incident ID not known"
      dl_ [] $ do
        esAttr es ["destination", "ip"]
        esAttr es ["event", "category"]
        esAttr es ["event", "severity"]
        esAttr es ["event", "start"]
        esAttr es ["incident", "description"]
        esAttr es ["observer", "name"]
        esAttr es ["trace", "id"]
  where
  esAttr es path = do
    maybeM_ (delve es ("_source":path)) $ jsonAsHtml (T.intercalate "." path)

maybeM_ :: (Monad m) => Maybe a -> (a -> m b) -> m ()
maybeM_ Nothing _ = pure ()
maybeM_ (Just a) f = f a >> pure ()

jsonAsHtml :: Text -> Json.Value -> Html ()
jsonAsHtml label = \case
  Json.String str -> withLabel $ do
    toHtml str
  Json.Number n
    | n == 0 -> pure ()
    | Right n <- Sci.floatingOrInteger n -> withLabel $ toHtml $ show @Int n
    | Left n <- Sci.floatingOrInteger n -> withLabel $ toHtml $ show @Double n
  _ -> undefined -- TODO
  where
  withLabel :: Html () -> Html ()
  withLabel content = do
    dt_ [] $ toHtml label
    dd_ [] content

delve :: Json.Value -> [Text] -> Maybe Json.Value
delve v0 path0 =Json.parseMaybe (loop path0) v0
  where
  loop [] = pure
  loop (prop:path) = withObject "" $ (loop path =<<) . (.: prop)


mkBodyOld :: Customer -> Alert -> [Notification] -> Text -> TL.Text
mkBodyOld Customer{name,kibanaIndexPattern}
    Alert{created,ruleDescription,ruleId,recordsHtml,traceIdentifier}
    notifications
    cleanRuleName = Lucid.renderText $ do
  h2_ $ toHtml $
    name <> " - Security Incident Report"
  p_ $ toHtml $
    Chronos.encode_YmdIMS_p EN.upper (SubsecondPrecisionFixed 0) Chronos.hyphen
      (Chronos.offsetDatetimeDatetime (Chronos.timeToOffsetDatetime (Offset (-300)) created)) -- FIXME: customer offset
  p_ $ h2_ $ toHtml cleanRuleName
  p_ $ toHtml ruleDescription
  p_ $ hyperlinkToTraceId traceIdentifier
  case kibanaIndexPattern of
    0 -> p_ $ hyperlinkToRelevant allEventsUuid created traceIdentifier
    pat -> p_ $ hyperlinkToRelevant pat created traceIdentifier
  h4_ [] "Attributes:"
  p_ $ details_ recordsHtml
  case List.find (\(Notification{id}) -> id == ruleId) notifications of
    Nothing -> pure ()
    Just Notification{playbooks,suggestedActions,referenceLinks} -> p_ $ do
      when (PM.sizeofArray playbooks /= 0) $ do
        h4_ "Playbooks:"
        forM_ playbooks $ \playbook -> do
          p_ $ toHtml playbook
      when (PM.sizeofArray suggestedActions /= 0) $ do
        h4_ "Suggested Actions:"
        forM_ suggestedActions $ \suggestedAction -> do
          p_ $ toHtml suggestedAction
      when (PM.sizeofArray referenceLinks /= 0) $ do
        h4_ "References:"
        forM_ referenceLinks $ \referenceLink -> do
          p_ $ toHtml referenceLink
