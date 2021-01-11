{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Functor ((<&>))
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)

import qualified Data.Aeson as Json
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Network.HTTP.Client as Http
import qualified Salesforce.Client as Sf
import qualified TheHive.CortexUtils as Hive


main :: IO ()
main = Hive.main Nothing $ \(Hive.Case input) -> do
  config <- maybe (error "invalid config") pure $ do
    config <- fromObject =<< HMap.lookup "config" input
    endpoint <-  fromText =<< HMap.lookup "endpoint" config
    clientId <- fromText =<< HMap.lookup "clientId" config
    clientSecret <- fromText =<< HMap.lookup "clientSecret" config
    username <- fromText =<< HMap.lookup "username" config
    password <- fromText =<< HMap.lookup "password" config
    pure Sf.Config{endpoint,clientId,clientSecret,username,password}
  newCase <- maybe (error "invalid data") pure $ do
    theData <- fromObject =<< HMap.lookup "data" input
    hiveCaseId <- fromInt =<< HMap.lookup "caseId" theData
    pure $ Json.Object $ HMap.fromList
          [ ("Subject", Json.String $ "Hive Case " <> T.pack (show hiveCaseId))
          , ("Description", Json.String $ (LT.toStrict . LT.decodeUtf8 . Json.encode) theData)
          ]
  sfManager <- Sf.newManager config
                <&> \x -> x{Sf.prettyPrint=True} -- DEBUG
  response <- Sf.post sfManager "sobjects/Case" newCase
  sfId <- maybe (error "unexpected salesforce reply") pure $ do
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
