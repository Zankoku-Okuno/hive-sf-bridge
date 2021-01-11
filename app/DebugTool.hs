{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Functor ((<&>))
import System.Environment (getEnv)

import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Network.HTTP.Client as Http
import qualified Salesforce.Client as Sf

main :: IO ()
main = do
  endpoint <- T.pack <$> getEnv "SALESFORCE_ENDPOINT"
  clientId <- T.pack <$> getEnv "SALESFORCE_CLIENTID"
  clientSecret <- T.pack <$> getEnv "SALESFORCE_CLIENTSECRET"
  username <- T.pack <$> getEnv "SALESFORCE_USERNAME"
  password <- T.pack <$> getEnv "SALESFORCE_PASSWORD"

  sfManager <- Sf.newManager Sf.Config{endpoint,clientId,clientSecret,username,password}
                <&> \x -> x{Sf.prettyPrint=True}
  -- print (accessToken sfManager)
  -- it <- get sfManager "sobjects/Case/describe"
  -- someCase <- fromJust . Json.decode @Json.Value . (LT.encodeUtf8 . LT.pack) <$> getContents
  -- print someCase
  -- it <- Sf.post sfManager "sobjects/Case" someCase
  it <- Sf.get sfManager "sobjects/Case/5004p00000IGXLvAAP"

  LT.putStrLn . LT.decodeUtf8 . Http.responseBody $ it
