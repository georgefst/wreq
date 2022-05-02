{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Network.Wreq.Internal.Lens
    (
      HTTP.Request
    , method
    , secure
    , host
    , port
    , path
    , queryString
    , requestHeaders
    , requestBody
    , requestVersion
    , requestManagerOverride
    , onRequestBodyException
    , proxy
    , hostAddress
    , rawBody
    , decompress
    , redirectCount
    , responseTimeout
    , checkResponse
    , cookieJar
    , seshCookies
    , seshManager
    , seshRun
    , seshRunHistory
    -- * Useful functions
    , assoc
    , assoc2
    , setHeader
    , maybeSetHeader
    , deleteKey
    ) where

import Control.Lens hiding (makeLenses)
import Data.List (partition)
import Network.HTTP.Client (Request)
import Network.HTTP.Types (HeaderName)
import Network.Wreq.Internal.Types (Session, Run, Body, RunHistory)
import qualified Data.ByteString as S
import qualified Network.HTTP.Client as HTTP

import qualified Network.HTTP.Client.Internal
import qualified Network.HTTP.Types as HTTP
import GHC.Generics (Generic)
import Control.Exception (SomeException)
import Data.Generics.Product (field)
import Data.IORef (IORef)
import Data.Word (Word32)

deriving instance Generic HTTP.Request

method :: Lens' HTTP.Request HTTP.Method
method = field @"method"
secure :: Lens' HTTP.Request Bool
secure = field @"secure"
host :: Lens' HTTP.Request S.ByteString
host = field @"host" @HTTP.Request @HTTP.Request
port :: Lens' HTTP.Request Int
port = field @"port"
path :: Lens' HTTP.Request S.ByteString
path = field @"path"
queryString :: Lens' HTTP.Request S.ByteString
queryString = field @"queryString"
requestHeaders :: Lens' HTTP.Request HTTP.RequestHeaders
requestHeaders = field @"requestHeaders"
requestBody :: Lens' HTTP.Request HTTP.RequestBody
requestBody = field @"requestBody"
proxy :: Lens' HTTP.Request (Maybe HTTP.Proxy)
proxy = field @"proxy"
hostAddress :: Lens' HTTP.Request (Maybe Word32)
hostAddress = field @"hostAddress"
rawBody :: Lens' HTTP.Request Bool
rawBody = field @"rawBody"
decompress :: Lens' HTTP.Request (S.ByteString -> Bool)
decompress = field @"decompress"
redirectCount :: Lens' HTTP.Request Int
redirectCount = field @"redirectCount"
checkResponse :: Lens' HTTP.Request (Request -> HTTP.Response HTTP.BodyReader -> IO ())
checkResponse = field @"checkResponse"
responseTimeout :: Lens' HTTP.Request HTTP.ResponseTimeout
responseTimeout = field @"responseTimeout"
cookieJar :: Lens' HTTP.Request (Maybe HTTP.CookieJar)
cookieJar = field @"cookieJar"
requestVersion :: Lens' HTTP.Request HTTP.HttpVersion
requestVersion = field @"requestVersion"
onRequestBodyException :: Lens' HTTP.Request ((SomeException -> IO ()))
onRequestBodyException = field @"onRequestBodyException"
requestManagerOverride :: Lens' HTTP.Request (Maybe HTTP.Manager)
requestManagerOverride = field @"requestManagerOverride"

seshCookies :: Lens' Session (Maybe (IORef HTTP.CookieJar))
seshCookies = field @"seshCookies"
seshManager :: Lens' Session HTTP.Manager
seshManager = field @"seshManager"
seshRun :: Lens' Session (Session -> Run Body -> Run Body)
seshRun = field @"seshRun"
seshRunHistory :: Lens' Session (Session -> RunHistory Body -> RunHistory Body)
seshRunHistory = field @"seshRunHistory"

assoc :: (Eq k) => k -> IndexedTraversal' k [(k, a)] a
assoc i = traverse . itraversed . index i

assoc2 :: Eq k => k -> Lens' [(k,a)] [a]
-- This is only a lens up to the ordering of the list (which changes
-- when we modify the list).
-- assoc2 :: (Eq b, Functor f) => b -> ([a] -> f [a]) -> [(b, a)] -> f [(b, a)]
assoc2 k f = fmap (uncurry ((++) . fmap ((,) k))) .
             _1 (f . fmap snd) . partition ((==k) . fst)

-- | Set a header to the given value, replacing any prior value.
setHeader :: HeaderName -> S.ByteString -> Request -> Request
setHeader name value = requestHeaders %~ ((name,value) :) . deleteKey name

-- | Set a header to the given value, but only if the header was not
-- already set.
maybeSetHeader :: HeaderName -> S.ByteString -> Request -> Request
maybeSetHeader name value = requestHeaders %~
  \hdrs -> case lookup name hdrs of
             Just _  -> hdrs
             Nothing -> (name,value) : hdrs

deleteKey :: (Eq a) => a -> [(a,b)] -> [(a,b)]
deleteKey key = filter ((/= key) . fst)
