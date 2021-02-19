{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Yuntan.Types.Internal
  ( Gateway (..)
  , initGateway
  , AppEnv (..)
  , DynamicSecret (..)
  , Method
  , Pathname
  ) where

import           Data.Aeson              (FromJSON (..), withObject, (.!=),
                                          (.:), (.:?))
import           Data.Int                (Int64)
import           Data.String.Utils       (startswith)
import           Network.HTTP.Client     (Manager, defaultManagerSettings,
                                          managerConnCount,
                                          managerResponseTimeout, newManager,
                                          responseTimeoutMicro)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

type Method = String
type Pathname = String

data Gateway opts = Gateway
  { host       :: String
  , appKey     :: String
  , appSecret  :: String
  , numThreads :: Int
  , timeout    :: Int
  , connCount  :: Int
  , mgr        :: Manager
  , mgrDyn     :: Manager
  , makeSecret :: Method -> Pathname -> IO DynamicSecret
  , appOpts    :: Maybe opts
  -- numThreads of fetch async for haxl
  }

instance Show (Gateway opts) where
  show a = concat [ "host = ", host a
                  , ", key = ", appKey a
                  , ", secret = ", appSecret a
                  ]

initGateway :: Gateway opts -> IO (Gateway opts)
initGateway gw@Gateway{..} = do
  mgr' <- newManager settings { managerConnCount = connCount
                              , managerResponseTimeout = responseTimeoutMicro $ timeout * 1000
                              }
  pure gw { mgr = mgr' }
  where settings = if startswith "https" host then tlsManagerSettings
                                              else defaultManagerSettings


instance FromJSON opts => FromJSON (Gateway opts) where
  parseJSON = withObject "Gateway" $ \o -> do
    host       <- o .:? "host"       .!= "https://gw.huabot.com"
    appKey     <- o .:  "key"
    appSecret  <- o .:? "secret"     .!= ""
    numThreads <- o .:? "numThreads" .!= 1
    timeout    <- o .:? "timeout"    .!= 30
    connCount  <- o .:? "conn-count" .!= 10
    appOpts    <- o .:? "options"
    return Gateway
      { mgr = error "uninitial"
      , mgrDyn = error "uninitial"
      , makeSecret = error "noimplement"
      , ..
      }

class AppEnv a where
  gateway :: a -> String -> (Gateway opts)

data DynamicSecret = DynamicSecret
  { nonce     :: String
  , secret    :: String
  , timestamp :: Int64
  }

instance FromJSON DynamicSecret where
  parseJSON = withObject "DynamicSecret" $ \o -> do
    nonce <- o .: "nonce"
    secret <- o .: "secret"
    timestamp <- o .: "timestamp"
    return DynamicSecret{..}
