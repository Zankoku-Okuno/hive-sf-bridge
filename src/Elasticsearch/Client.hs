{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Elasticsearch.Client
  ( Config(..)
  , Manager(..)
  , newManager
  , get
  ) where

import Network.HTTP.Client (parseRequest,httpLbs)
import Network.HTTP.Client.TLS (newTlsManagerWith)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Connection as Connection
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Tls


data Config = Config
  { endpoint :: !String
  , username :: !String
  , password :: !String
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
get Manager{httpsManager,_config=Config{endpoint,username,password}} resource = do
  request <- parseRequest theUrl
  httpLbs request httpsManager
  where
  theUrl = concat ["https://", username, ":", password, "@", endpoint, resource]

noVerifyTlsManagerSettings :: Http.ManagerSettings
noVerifyTlsManagerSettings = Tls.mkManagerSettings noVerifyTlsSettings Nothing
noVerifyTlsSettings :: Connection.TLSSettings
noVerifyTlsSettings = Connection.TLSSettingsSimple
  { Connection.settingDisableCertificateValidation = True
  , Connection.settingDisableSession = True
  , Connection.settingUseServerName = False
  }
