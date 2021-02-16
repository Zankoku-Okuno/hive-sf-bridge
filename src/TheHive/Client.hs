{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TheHive.Client
  ( Config(..)
  , Manager(..)
  , newManager
  , get
  , post
  ) where

import Data.Functor ((<&>))
import Data.Text (Text)
import Network.HTTP.Client (parseRequest,httpLbs)
import Network.HTTP.Client.TLS (newTlsManagerWith)

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T
import qualified Network.Connection as Connection
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Tls


data Config = Config
  { endpoint :: !String
  , apikey :: !Text
  }

data Manager = Manager
  { httpsManager :: !Http.Manager
  , _config :: Config
  }

newManager :: Config -> IO Manager
newManager _config = do
  httpsManager <- newTlsManagerWith noVerifyTlsManagerSettings
  pure Manager{httpsManager,_config}

get :: Manager -> String -> IO (Http.Response LBS.ByteString)
get Manager{httpsManager,_config=Config{endpoint,apikey}} resource = do
  request <- parseRequest (concat ["https://", endpoint, resource])
    <&> \r -> r{Http.requestHeaders =
      [ ("Authorization", "Bearer " <> T.encodeUtf8 apikey)
      , ("Content-Type", "application/json;charset=utf-8")
      ]
    }
  httpLbs request httpsManager

post :: Manager -> String -> Json.Value -> IO (Http.Response LBS.ByteString)
post Manager{httpsManager,_config=Config{endpoint,apikey}} resource json = do
  request <- parseRequest (concat ["POST ", "https://", endpoint, resource])
    <&> \r -> r
    { Http.requestHeaders =
      [ ("Authorization", "Bearer " <> T.encodeUtf8 apikey)
      , ("Content-Type", "application/json;charset=utf-8")
      ]
    , Http.requestBody = Http.RequestBodyLBS $ Json.encode json
    }
  httpLbs request httpsManager

noVerifyTlsManagerSettings :: Http.ManagerSettings
noVerifyTlsManagerSettings = Tls.mkManagerSettings noVerifyTlsSettings Nothing
noVerifyTlsSettings :: Connection.TLSSettings
noVerifyTlsSettings = Connection.TLSSettingsSimple
  { Connection.settingDisableCertificateValidation = True
  , Connection.settingDisableSession = True
  , Connection.settingUseServerName = False
  }
