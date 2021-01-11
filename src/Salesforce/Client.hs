{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Salesforce.Client
  ( AccessToken
  , Config(..)
  , Manager(..)
  , newManager
  , get
  , post
  ) where

import Data.Aeson (FromJSON, (.:))
import Data.Functor ((<&>))
import Data.Text (Text)
import Network.HTTP.Client (parseRequest,setQueryString,httpLbs)
import Network.HTTP.Client.TLS (newTlsManager)

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client as Http


data AccessToken = AccessToken {unAccessToken :: Text}
  deriving (Show)

------------------ Setup ------------------

data Config = Config
  { endpoint :: Text
  , clientId :: Text
  , clientSecret :: Text
  , username :: Text
  , password :: Text
  }

data Manager = Manager
  { httpsManager :: Http.Manager
  , accessToken :: AccessToken
  , endpoint :: Text
  , prettyPrint :: Bool
  }

newManager :: Config -> IO Manager
newManager Config{endpoint,clientId,clientSecret,username,password} = do
  httpsManager <- newTlsManager
  request <- (parseRequest $ "POST https://" ++ T.unpack endpoint ++ "/services/oauth2/token")
    <&> setQueryString
      [ ("grant_type", Just "password")
      , ("client_id", Just $ T.encodeUtf8 clientId)
      , ("client_secret", Just $ T.encodeUtf8 clientSecret)
      , ("username", Just $ T.encodeUtf8 username)
      , ("password", Just $ T.encodeUtf8 password)
      ]
  response <- httpLbs request httpsManager
  -- FIXME check for error responses

  case Json.decode $ Http.responseBody response of
    Just accessToken -> pure $ Manager
      { httpsManager, accessToken, endpoint
      , prettyPrint = False
      }
    Nothing -> error "attempt to get salseforce access token failed when extracting from json"


------------------ Requests ------------------

get :: Manager -> String -> IO (Http.Response LBS.ByteString)
get manager@Manager{endpoint} resource = do
  request <- parseRequest theUrl
    <&> \r -> r{Http.requestHeaders =
      [ ("Authorization"
        , "Bearer " <> (T.encodeUtf8 . unAccessToken . accessToken) manager)
      ]
      ++ if prettyPrint manager then [("X-PrettyPrint", "1")] else []
    }
  httpLbs request (httpsManager manager)
  where
  theUrl = "https://" ++ T.unpack endpoint ++ "/services/data/v44.0/" ++ resource

post :: Manager -> String -> Json.Value -> IO (Http.Response LBS.ByteString)
post manager@Manager{httpsManager,endpoint} resource json = do
  request <- parseRequest ("POST " ++ theUrl) <&> setAuth . setPP . setJsonBody
  httpLbs request httpsManager
  where
  theUrl = "https://" ++ T.unpack endpoint ++ "/services/data/v44.0/" ++ resource
  setAuth req = req{Http.requestHeaders = header : Http.requestHeaders req}
    where
    header = ("Authorization"
             , "Bearer " <> (T.encodeUtf8 . unAccessToken . accessToken) manager
             )
  setPP req = if prettyPrint manager
    then req{Http.requestHeaders = ("X-PrettyPrint", "1") : Http.requestHeaders req}
    else req
  setJsonBody req = req
    { Http.requestHeaders = ("Content-Type", "application/json") : Http.requestHeaders req
    , Http.requestBody = Http.RequestBodyLBS $ Json.encode json
    }


------------------ Codecs ------------------

instance FromJSON AccessToken where
  parseJSON = Json.withObject "AccessToken" $ \v ->
    AccessToken <$> v .: "access_token"
