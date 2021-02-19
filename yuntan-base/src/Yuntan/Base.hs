{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yuntan.Base
  (
    module X
  , getOptions
  , getOptionsAndSign
  , getOptionsAndSignJSON
  , getOptionsAndSignRaw
  ) where

import           Yuntan.Types.Internal as X

import           Control.Lens          ((&), (.~))
import           Crypto.Signature      (signJSON, signParams, signRaw)
import           Data.Aeson            (Value (..))
import qualified Data.ByteString.Char8 as B (ByteString, pack)
import           Data.CaseInsensitive  (original)
import           Data.HashMap.Strict   (insert)
import           Data.Text             (pack)
import qualified Data.Text.Lazy        as LT (Text, pack)
import           Data.UnixTime
import           Network.HTTP.Client   (Manager)
import           Network.Wreq          (Options, defaults, header, manager)

getMgr :: Manager -> Options
getMgr mgr = defaults & manager .~ Right mgr

getOptions :: Gateway opts -> Options
getOptions Gateway{appKey = key, mgr = mgr} =
  getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
             & header "User-Agent" .~ ["haskell yuntan-base-0.1.0.0"]

prepare :: (Pathname -> String -> String -> a -> Gateway opts -> IO Options) -> Method -> Pathname -> a -> Gateway opts -> IO Options
prepare done method pathname params gw@Gateway{appSecret=[]} = do
  DynamicSecret {..} <- makeSecret gw method pathname
  opts <- done pathname (show timestamp) secret params gw
  pure $ opts & header "X-REQUEST-NONCE" .~ [B.pack nonce]
              & header "X-REQUEST-TYPE" .~ ["JSAPI"]

prepare done _ pathname params gw@Gateway{appSecret=sec} = do
  t <- show . toEpochTime <$> getUnixTime
  done pathname t sec params gw

getOptionsAndSign_ :: Pathname -> String -> String -> [(LT.Text, LT.Text)] -> Gateway opts -> IO Options
getOptionsAndSign_ pathname ts sec params Gateway{appKey = key, mgr = mgr} = do
  let sign = signParams (B.pack sec) (("pathname", LT.pack pathname):
                                      ("timestamp", LT.pack ts):
                                      ("key", LT.pack key):params)
      opts = getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
                        & header "X-REQUEST-SIGNATURE" .~ [original sign]
                        & header "X-REQUEST-TIME" .~ [B.pack ts]
                        & header "User-Agent" .~ ["haskell yuntan-base-0.1.0.0"]
  return opts

getOptionsAndSign = prepare getOptionsAndSign_

getOptionsAndSignJSON_ :: Pathname -> String -> String -> Value -> Gateway opts -> IO Options
getOptionsAndSignJSON_ pathname ts sec (Object v) Gateway{appKey = key, mgr = mgr} = do
  let v'   = insert "timestamp" (String $ pack ts)
           $ insert "pathname" (String $ pack pathname)
           $ insert "key" (String $ pack key) v
      sign = signJSON (B.pack sec) (Object v')
      opts = getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
                        & header "X-REQUEST-SIGNATURE" .~ [original sign]
                        & header "X-REQUEST-TIME" .~ [B.pack ts]
                        & header "User-Agent" .~ ["haskell yuntan-base-0.1.0.0"]
                        & header "Content-Type" .~ ["application/json"]
  return opts

getOptionsAndSignJSON_ _ _ _ (Array _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON_ _ _ _ (String _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON_ _ _ _ (Number _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON_ _ _ _ (Bool _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON_ _ _ _ Null _ = error "Unsupport Aeson.Value signature"

getOptionsAndSignJSON = prepare getOptionsAndSignJSON_

getOptionsAndSignRaw_ :: Pathname -> String -> String -> B.ByteString -> Gateway opts -> IO Options
getOptionsAndSignRaw_ path ts sec dat Gateway{appKey = key, mgr = mgr} = do
  let sign = signRaw (B.pack sec) [ ("key", B.pack key)
                                  , ("timestamp", B.pack ts)
                                  , ("raw", dat)
                                  , ("pathname", B.pack path)
                                  ]
      opts = getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
                        & header "X-REQUEST-SIGNATURE" .~ [original sign]
                        & header "X-REQUEST-TIME" .~ [B.pack ts]
                        & header "User-Agent" .~ ["haskell yuntan-base-0.1.0.0"]
  return opts

getOptionsAndSignRaw = prepare getOptionsAndSignRaw_
