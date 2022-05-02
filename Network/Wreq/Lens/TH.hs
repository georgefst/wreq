{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.Wreq.Lens.TH
    (
      Types.Options
    , manager
    , proxy
    , auth
    , header
    , headers
    , param
    , params
    , redirects
    , cookie
    , cookies
    , checkResponse

    , HTTP.Cookie
    , cookieName
    , cookieValue
    , cookieExpiryTime
    , cookieDomain
    , cookiePath
    , cookieCreationTime
    , cookieLastAccessTime
    , cookiePersistent
    , cookieHostOnly
    , cookieSecureOnly
    , cookieHttpOnly

    , HTTP.Proxy
    , proxyHost
    , proxyPort

    , HTTP.Response
    , responseStatus
    , responseVersion
    , responseHeader
    , responseHeaders
    , responseLink
    , responseBody
    , responseCookie
    , responseCookieJar
    , responseClose'

    , HTTP.HistoriedResponse
    , hrFinalResponse
    , hrFinalRequest
    , hrRedirects

    , HTTP.Status
    , statusCode
    , statusMessage

    , Types.Link
    , linkURL
    , linkParams

    , Form.PartM
    , partName
    , partFilename
    , partContentType
    , partGetBody
    , partHeaders
    ) where

import Control.Lens hiding (makeLenses)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.Wreq.Internal.Lens (assoc, assoc2)
import Network.Wreq.Internal.Link
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.MultipartFormData as Form
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wreq.Types as Types

import Data.Generics.Product (field)
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import qualified Network.HTTP.Client.Internal as HTTP
import qualified Data.ByteString.Lazy as BL
import Network.Mime (MimeType)
import Network.HTTP.Types (HttpVersion)

deriving instance Generic HTTP.Cookie
deriving instance Generic HTTP.Proxy
deriving instance Generic (HTTP.Response body)
deriving instance Generic HTTP.Status
deriving instance Generic Types.Link

manager :: Lens' Types.Options (Either HTTP.ManagerSettings HTTP.Manager)
manager = field @"manager"
proxy :: Lens' Types.Options (Maybe HTTP.Proxy)
proxy = field @"proxy"
auth :: Lens' Types.Options (Maybe Types.Auth)
auth = field @"auth"
headers :: Lens' Types.Options [HTTP.Header]
headers = field @"headers"
params :: Lens' Types.Options [(Text, Text)]
params = field @"params"
redirects :: Lens' Types.Options Int
redirects = field @"redirects"
cookies :: Lens' Types.Options (Maybe HTTP.CookieJar)
cookies = field @"cookies"
checkResponse :: Lens' Types.Options (Maybe Types.ResponseChecker)
checkResponse = field @"checkResponse"

cookieName :: Lens' HTTP.Cookie ByteString
cookieName = field @"cookie_name"
cookieValue :: Lens' HTTP.Cookie ByteString
cookieValue = field @"cookie_value"
cookieExpiryTime :: Lens' HTTP.Cookie UTCTime
cookieExpiryTime = field @"cookie_expiry_time"
cookieDomain :: Lens' HTTP.Cookie ByteString
cookieDomain = field @"cookie_domain"
cookiePath :: Lens' HTTP.Cookie ByteString
cookiePath = field @"cookie_path"
cookieCreationTime :: Lens' HTTP.Cookie UTCTime
cookieCreationTime = field @"cookie_creation_time"
cookieLastAccessTime :: Lens' HTTP.Cookie UTCTime
cookieLastAccessTime = field @"cookie_last_access_time"
cookiePersistent :: Lens' HTTP.Cookie Bool
cookiePersistent = field @"cookie_persistent"
cookieHostOnly :: Lens' HTTP.Cookie Bool
cookieHostOnly = field @"cookie_host_only"
cookieSecureOnly :: Lens' HTTP.Cookie Bool
cookieSecureOnly = field @"cookie_secure_only"
cookieHttpOnly :: Lens' HTTP.Cookie Bool
cookieHttpOnly = field @"cookie_http_only"

proxyHost :: Lens' HTTP.Proxy ByteString
proxyHost = field @"proxyHost"
proxyPort :: Lens' HTTP.Proxy Int
proxyPort = field @"proxyPort"

responseStatus :: Lens' (HTTP.Response body) HTTP.Status
responseStatus = field @"responseStatus"
responseVersion :: Lens' (HTTP.Response body) HttpVersion
responseVersion = field @"responseVersion"
responseHeaders :: Lens' (HTTP.Response body) [HTTP.Header]
responseHeaders = field @"responseHeaders"
responseBody :: Lens (HTTP.Response body0) (HTTP.Response body1) body0 body1
responseBody = field @"responseBody"
responseCookieJar :: Lens' (HTTP.Response body) HTTP.CookieJar
responseCookieJar = field @"responseCookieJar"
responseClose' :: Lens' (HTTP.Response body) HTTP.ResponseClose
responseClose' = field @"responseClose'"

hrFinalResponse :: Lens' (HTTP.HistoriedResponse body) (HTTP.Response body)
hrFinalResponse = field @"hrFinalResponse"
hrFinalRequest :: Lens' (HTTP.HistoriedResponse body) HTTP.Request
hrFinalRequest = field @"hrFinalRequest"
hrRedirects :: Lens' (HTTP.HistoriedResponse body) [(HTTP.Request, HTTP.Response BL.ByteString)]
hrRedirects = field @"hrRedirects"

statusCode :: Lens' HTTP.Status Int
statusCode = field @"statusCode"
statusMessage :: Lens' HTTP.Status ByteString
statusMessage = field @"statusMessage"

linkURL :: Lens' Types.Link ByteString
linkURL = field @"linkURL"
linkParams :: Lens' Types.Link [(ByteString, ByteString)]
linkParams = field @"linkParams"

partName :: Lens' (Form.PartM m) Text
partName = _
partFilename :: Lens' (Form.PartM m) (Maybe String)
partFilename = _
partContentType :: Lens' (Form.PartM m) (Maybe MimeType)
partContentType = _
partGetBody :: Lens' (Form.PartM m) (m HTTP.RequestBody)
partGetBody = _
partHeaders :: Lens' (Form.PartM m) [HTTP.Header]
partHeaders = _

responseHeader :: HTTP.HeaderName -> Traversal' (HTTP.Response body) ByteString
responseHeader n = responseHeaders . assoc n

param :: Text -> Lens' Types.Options [Text]
param n = params . assoc2 n

header :: HTTP.HeaderName -> Lens' Types.Options [ByteString]
header n = headers . assoc2 n

_CookieJar :: Iso' HTTP.CookieJar [HTTP.Cookie]
_CookieJar = iso HTTP.destroyCookieJar HTTP.createCookieJar

-- N.B. This is an "illegal" traversal because we can change its cookie_name.
cookie :: ByteString -> Traversal' Types.Options HTTP.Cookie
cookie name = cookies . _Just . _CookieJar . traverse . filtered
              (\c -> HTTP.cookie_name c == name)

responseCookie :: ByteString -> Fold (HTTP.Response body) HTTP.Cookie
responseCookie name =
  responseCookieJar . folding HTTP.destroyCookieJar . filtered
  ((==name) . HTTP.cookie_name)

responseLink :: ByteString -> ByteString -> Fold (HTTP.Response body) Types.Link
responseLink name val =
  responseHeader "Link" . folding links .
  filtered (has (linkParams . folded . filtered (== (name,val))))
