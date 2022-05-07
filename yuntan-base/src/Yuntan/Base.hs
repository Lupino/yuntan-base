{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yuntan.Base
  ( module X
  , Options
  , addHeader
  , setContentType
  , getOptions
  , getOptionsAndSign
  , getOptionsAndSignJSON
  , getOptionsAndSignRaw
  , httpLbs
  , getWith
  , postWith
  , postLbsWith
  , deleteWith
  , putWith
  , putLbsWith
  , customPayloadMethodWith
  , customPayloadLbsMethodWith
  , customMethodWith
  , (.=)

  , Request
  , Response
  ) where

import           Crypto.Signature      (signJSON, signParams, signRaw)
import           Data.Aeson            (Value (..))
import           Data.Aeson.KeyMap     (insert)
import qualified Data.ByteString.Char8 as B (ByteString, pack)
import qualified Data.ByteString.Lazy  as LB (ByteString)
import           Data.CaseInsensitive  (original)
import           Data.List             (nubBy)
import           Data.Text             (pack)
import qualified Data.Text.Lazy        as LT (Text, pack)
import           Data.UnixTime
import           Network.HTTP.Client   (Manager, Request, RequestBody (..),
                                        Response, method, parseRequest,
                                        requestBody, requestHeaders,
                                        urlEncodedBody)
import qualified Network.HTTP.Client   as HTTP (httpLbs)
import           Network.HTTP.Types    (Header, RequestHeaders, methodDelete,
                                        methodGet, methodPost, methodPut)
import           Yuntan.Types.Internal as X

data Options = Options
  { manager :: Manager
  , headers :: RequestHeaders
  }


addHeader :: Header -> Options -> Options
addHeader h opts = opts
  { headers = h:headers opts
  }


setContentType :: B.ByteString -> Options -> Options
setContentType v = addHeader ("Content-Type", v)

getOptions :: Gateway opts -> Options
getOptions Gateway{appKey = key, mgr = mgr} = Options
  { manager = mgr
  , headers =
    [ "X-REQUEST-KEY" .= B.pack key
    , "User-Agent"    .= "haskell yuntan-base-0.1.0.0"
    ]
  }

prepare
  :: (Pathname -> String -> String -> a -> Gateway opts -> Options)
  -> Method -> Pathname -> a -> Gateway opts -> IO Options
prepare done method pathname params gw@Gateway{appSecret=[]} = do
  DynamicSecret {..} <- makeSecret gw method pathname
  let opts = (done pathname (show timestamp) secret params gw)
  pure opts
    { headers = headers opts ++
      [ "X-REQUEST-NONCE" .= B.pack nonce
      , "X-REQUEST-TYPE"  .= "JSAPI"
      ]
    }

prepare done _ pathname params gw@Gateway{appSecret=sec} = do
  t <- show . toEpochTime <$> getUnixTime
  pure $ done pathname t sec params gw

getOptionsAndSign_ :: Pathname -> String -> String -> [(LT.Text, LT.Text)] -> Gateway opts -> Options
getOptionsAndSign_ pathname ts sec params Gateway{appKey = key, mgr = mgr} = Options
  { manager = mgr
  , headers =
    [ "X-REQUEST-KEY"       .= B.pack key
    , "X-REQUEST-SIGNATURE" .= original sign
    , "X-REQUEST-TIME"      .= B.pack ts
    , "User-Agent"          .= "haskell yuntan-base-0.1.0.0"
    ]
  }
  where sign = signParams (B.pack sec) $
          [ "pathname"  .= LT.pack pathname
          , "timestamp" .= LT.pack ts
          , "key"       .= LT.pack key
          ] ++ params

getOptionsAndSign = prepare getOptionsAndSign_

getOptionsAndSignJSON_ :: Pathname -> String -> String -> Value -> Gateway opts -> Options
getOptionsAndSignJSON_ pathname ts sec (Object v) Gateway{appKey = key, mgr = mgr} = Options
  { manager = mgr
  , headers =
    [ "X-REQUEST-KEY"       .= B.pack key
    , "X-REQUEST-SIGNATURE" .= original sign
    , "X-REQUEST-TIME"      .= B.pack ts
    , "User-Agent"          .= "haskell yuntan-base-0.1.0.0"
    , "Content-Type"        .= "application/json"
    ]
  }
  where
    v'   = insert "timestamp" (String $ pack ts)
         $ insert "pathname" (String $ pack pathname)
         $ insert "key" (String $ pack key) v
    sign = signJSON (B.pack sec) (Object v')

getOptionsAndSignJSON_ _ _ _ (Array _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON_ _ _ _ (String _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON_ _ _ _ (Number _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON_ _ _ _ (Bool _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON_ _ _ _ Null _ = error "Unsupport Aeson.Value signature"

getOptionsAndSignJSON = prepare getOptionsAndSignJSON_

getOptionsAndSignRaw_ :: Pathname -> String -> String -> B.ByteString -> Gateway opts -> Options
getOptionsAndSignRaw_ path ts sec dat Gateway{appKey = key, mgr = mgr} = Options
  { manager = mgr
  , headers =
    [ "X-REQUEST-KEY"       .= B.pack key
    , "X-REQUEST-SIGNATURE" .= original sign
    , "X-REQUEST-TIME"      .= B.pack ts
    , "User-Agent"          .= "haskell yuntan-base-0.1.0.0"
    ]
  }
  where sign = signRaw (B.pack sec)
          [ "key"       .= B.pack key
          , "timestamp" .= B.pack ts
          , "raw"       .= dat
          , "pathname"  .= B.pack path
          ]

getOptionsAndSignRaw = prepare getOptionsAndSignRaw_

httpLbs :: Request -> Options -> IO (Response LB.ByteString)
httpLbs req Options {..} =
  HTTP.httpLbs req { requestHeaders = nubBy nubFunc reqHeaders } manager
  where reqHeaders = headers ++ requestHeaders req
        nubFunc :: Header -> Header -> Bool
        nubFunc h0 h1 = fst h0 == fst h1

customPayloadMethodWith
  :: Method -> Options -> String -> [(B.ByteString, B.ByteString)]
  -> IO (Response LB.ByteString)
customPayloadMethodWith m opts url params = do
  req <- urlEncodedBody params <$> parseRequest url
  httpLbs req {method = m} opts

customPayloadLbsMethodWith
  :: Method -> Options -> String -> LB.ByteString
  -> IO (Response LB.ByteString)
customPayloadLbsMethodWith m opts url lbs = do
  req <- parseRequest url
  httpLbs req {method = m, requestBody = RequestBodyLBS lbs} opts

customMethodWith :: Method -> Options -> String -> IO (Response LB.ByteString)
customMethodWith m opts url = do
  req <- parseRequest url
  httpLbs req {method = m} opts

getWith :: Options -> String -> IO (Response LB.ByteString)
getWith = customMethodWith methodGet

postWith :: Options -> String -> [(B.ByteString, B.ByteString)] -> IO (Response LB.ByteString)
postWith = customPayloadMethodWith methodPost

postLbsWith :: Options -> String -> LB.ByteString -> IO (Response LB.ByteString)
postLbsWith = customPayloadLbsMethodWith methodPost

deleteWith :: Options -> String -> IO (Response LB.ByteString)
deleteWith = customMethodWith methodDelete

putWith :: Options -> String -> [(B.ByteString, B.ByteString)] -> IO (Response LB.ByteString)
putWith = customPayloadMethodWith methodPut

putLbsWith :: Options -> String -> LB.ByteString -> IO (Response LB.ByteString)
putLbsWith = customPayloadLbsMethodWith methodPut

infixr 3 .=

(.=) :: a -> b -> (a, b)
(.=) a b = (a, b)
