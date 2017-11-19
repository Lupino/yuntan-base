{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yuntan.Base
  (
    module X
  , getOptions
  , getOptionsAndSign
  , getOptionsAndSignJSON
  , getOptionsAndSignRaw
  ) where

import           Yuntan.Types.Internal  as X

import           Control.Lens           ((&), (.~))
import           Data.Aeson             (Value (..))
import qualified Data.ByteString.Char8  as B (ByteString, pack)
import           Data.HashMap.Strict    (insert)
import           Data.Text              (pack)
import qualified Data.Text.Lazy         as LT (Text, pack)
import           Data.UnixTime
import           Network.HTTP.Client    (Manager)
import           Network.Wreq           (Options, defaults, header, manager)
import           Yuntan.Types.Internal  (Gateway (..))
import           Yuntan.Utils.Signature (signJSON, signParams, signRaw)


getMgr :: Maybe Manager -> Options
getMgr Nothing    = defaults
getMgr (Just mgr) = defaults & manager .~ Right mgr

getOptions :: Gateway -> Options
getOptions Gateway{getGWAppKey = key, getGWMgr = mgr} =
  getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
             & header "User-Agent" .~ ["haskell yuntan-base-0.1.0.0"]

getOptionsAndSign :: [(LT.Text, LT.Text)] -> Gateway -> IO Options
getOptionsAndSign params Gateway{getGWAppKey = key, getGWAppSecret = sec, getGWMgr = mgr} = do
  t <- show . toEpochTime <$> getUnixTime
  let sign = signParams (B.pack sec) (("timestamp", LT.pack t):("key", LT.pack key):params)
      opts = getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
                        & header "X-REQUEST-SIGNATURE" .~ [sign]
                        & header "X-REQUEST-TIME" .~ [B.pack t]
                        & header "User-Agent" .~ ["haskell yuntan-base-0.1.0.0"]
  return opts

getOptionsAndSignJSON :: Value -> Gateway -> IO Options
getOptionsAndSignJSON (Object v) Gateway{getGWAppKey = key, getGWAppSecret = sec, getGWMgr = mgr} = do
  t <- show . toEpochTime <$> getUnixTime
  let v'   = insert "timestamp" (String $ pack t) $ insert "key" (String $ pack key) v
      sign = signJSON (B.pack sec) (Object v')
      opts = getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
                        & header "X-REQUEST-SIGNATURE" .~ [sign]
                        & header "X-REQUEST-TIME" .~ [B.pack t]
                        & header "User-Agent" .~ ["haskell yuntan-base-0.1.0.0"]
                        & header "Content-Type" .~ ["application/json"]
  return opts

getOptionsAndSignJSON (Array _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON (String _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON (Number _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON (Bool _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON Null _ = error "Unsupport Aeson.Value signature"

getOptionsAndSignRaw :: String -> B.ByteString -> Gateway -> IO Options
getOptionsAndSignRaw path dat Gateway{getGWAppKey = key, getGWAppSecret = sec, getGWMgr = mgr} = do
  t <- show . toEpochTime <$> getUnixTime
  let sign = signRaw (B.pack sec) [ ("key", B.pack key)
                                  , ("timestamp", B.pack t)
                                  , ("raw", dat)
                                  , ("pathname", B.pack path)
                                  ]
      opts = getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
                        & header "X-REQUEST-SIGNATURE" .~ [sign]
                        & header "X-REQUEST-TIME" .~ [B.pack t]
                        & header "User-Agent" .~ ["haskell yuntan-base-0.1.0.0"]
  return opts
