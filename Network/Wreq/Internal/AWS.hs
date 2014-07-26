{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Wreq.Internal.AWS
    (
      signRequest
    , addTmpPayloadHashHeader
    ) where

import Control.Lens ((%~), (^.))
import Crypto.MAC (hmac, hmacGetDigest)
import Data.ByteString.Base16 as HEX (encode)
import Data.Byteable (toBytes)
import Data.Char (toLower)
import Data.List (sort, sortBy)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (utc, utcToLocalTime)
import Network.HTTP.Client (Request)
import Network.HTTP.Types (parseSimpleQuery, urlEncode)
import Network.Wreq.Internal.Lens (requestHeaders, setHeader, method, path, queryString, host)
import System.Locale (defaultTimeLocale)
import qualified Crypto.Hash as CT (HMAC, SHA256)
import qualified Crypto.Hash.SHA256 as SHA256 (hash, hashlazy)
import qualified Data.ByteString.Char8 as S
import qualified Data.CaseInsensitive  as CI (CI, original)
import qualified Data.HashSet as HashSet
import qualified Network.HTTP.Client as HTTP

-- Sign requests following the AWS v4 request signing specification:
-- http://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html
--
-- Q: how do we get the payload hash to the signRequest function?
--
-- A: we use a (temporary) HTTP header to 'tunnel' the payload hash to
-- the signing function.  For POST and PUT requests, the
-- Network.Wreq.Types.payload function adds a HTTP header (name
-- defined in 'tmpPayloadHashHeader'). The
-- Network.Wreq.Internal.AWS.signRequest function reads the value of
-- the header and then removes it from the request.  For GET, HEAD,
-- and (currently) DELETE that carry no body, we use "" per AWS
-- documentation Item 6: "use empty string" in
-- http://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html

-- TODO: adjust when DELETE supports a body or PATCH is added
signRequest :: S.ByteString -> S.ByteString -> Request -> IO Request
signRequest key secret request = do
  ts <- timestamp                         -- YYYYMMDDT242424Z, UTC based
  let (service, region) = serviceAndRegion $ req ^. host
      date = S.takeWhile (/= 'T') ts      -- YYYYMMDD
      hashedPayload
        | request ^. method `elem` ["POST", "PUT"] =
          fromJust . lookup tmpPayloadHashHeader $ request ^. requestHeaders
        | otherwise = HEX.encode $ SHA256.hash ""
      serviceHeaders =
        [("x-amz-content-sha256", hashedPayload) | service == "s3"]
      -- add common v4 signing headers, service specific headers and drop tmp header
      req = (requestHeaders %~
               ( ( [ ("host", request ^. host), ("x-amz-date", ts) ] ++ serviceHeaders ) ++)
             . filter ((/= tmpPayloadHashHeader) . fst) -- drop tmp header
            ) request
  -- task 1
  let hl = req ^. requestHeaders
      signedHeaders = S.intercalate ";" . sort . map (lowerCI . fst) $ hl
      canonicalReq = S.intercalate "\n" [
          req ^. method             -- step 1
        , req ^. path               -- step 2
        ,   S.intercalate "&"       -- step 3b, incl. sort
          . map (\(k,v) -> S.intercalate "=" . map (urlEncode False) $ [k,v])
          . sortBy (comparing fst) $
          parseSimpleQuery $ req ^. queryString
        ,   S.concat                -- step 4, incl. sort
          . map (  ( <> "\n")       -- append, not intercalate!
                 . (\(k,v) -> S.intercalate ":" [ lowerCI k, trimHeaderValue v ]))
          . sortBy (comparing fst) $ hl
        , signedHeaders             -- step 5
        , hashedPayload             -- step 6, handles empty payload
        ]
  -- task 2
  let dateScope = S.intercalate "/" [date, region, service, "aws4_request"]
      stringToSign = S.intercalate "\n" [
          "AWS4-HMAC-SHA256"
        , ts
        , dateScope
        , HEX.encode $ SHA256.hash canonicalReq
        ]
  -- task 3, step 1
  let kDate    = hmac' date ("AWS4" <> secret)
      kRegion  = hmac' region kDate
      kService = hmac' service kRegion
      kSigning = hmac' "aws4_request" kService
  -- task 3, step 2
  let signature = HEX.encode $ hmac' stringToSign kSigning
      authorization = S.intercalate ", " [
          "AWS4-HMAC-SHA256 Credential=" <> key <> "/" <> dateScope
        , "SignedHeaders=" <> signedHeaders
        , "Signature=" <> signature
        ]
  return $ setHeader "Authorization" authorization req
  where
    lowerCI :: CI.CI S.ByteString -> S.ByteString
    lowerCI = S.map toLower . CI.original
    trimHeaderValue =
      id -- FIXME, see step 4, whitespace trimming but not in double quoted sections, AWS spec.
    timestamp = do
      ts <- getCurrentTime -- UTC
      let local = utcToLocalTime utc ts -- UTC printable: YYYYMMDDTHHMMSSZ
      return . S.pack $ formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" local
    hmac' :: S.ByteString -> S.ByteString -> S.ByteString
    hmac' s k = let h = hmac k s :: (CT.HMAC CT.SHA256)
                  in toBytes $ hmacGetDigest h

addTmpPayloadHashHeader :: Request -> IO Request
addTmpPayloadHashHeader req = do
  -- Causes a warning because we don't support e.g. Streaming -
  -- preferring the warning over adding an explicit error case. The BS
  -- and LBS support matches Network.Wreq.Types(Putable).
  let payloadHash = case HTTP.requestBody req of
        HTTP.RequestBodyBS bs ->
          HEX.encode $ SHA256.hash bs
        HTTP.RequestBodyLBS lbs ->
          HEX.encode $ SHA256.hashlazy lbs
  return $ setHeader tmpPayloadHashHeader payloadHash req

tmpPayloadHashHeader :: CI.CI S.ByteString
tmpPayloadHashHeader = "X-LOCAL-CONTENT-HASH-HEADER-746352" -- 746352 to reduce collision risk

-- Per AWS documentation at:
--   http://docs.aws.amazon.com/general/latest/gr/rande.html
-- For example: "dynamodb.us-east-1.amazonaws.com" -> ("dynamodb", "us-east-1")
serviceAndRegion :: S.ByteString -> (S.ByteString, S.ByteString)
serviceAndRegion endpoint =
  -- For s3, use /<bucket> style access, as opposed to <bucket>.s3... in the hostname.
  if "s3.amazonaws.com" == endpoint || "s3-external-1.amazonaws.com" == endpoint then
    (s3, usEast1)
  else
    if s3 == servicePrefix '-' endpoint then -- format: e.g. s3-us-west-2.amazonaws.com
      let region = S.takeWhile (/= '.') $ S.drop 3 endpoint -- drop "s3-"
      in (s3, region)
    else
      -- not s3
      let svc = servicePrefix '.' endpoint
      in if svc `HashSet.member` noRegion then
           (svc, usEast1)
         else
           let service:region:_ = S.split '.' endpoint
           in (service, region)
  where
    usEast1 = "us-east-1"
    s3 = "s3"
    servicePrefix c = S.map toLower . S.takeWhile (/= c)
    noRegion = HashSet.fromList [ "iam", "sts", "importexport", "route53", "cloudfront"]
