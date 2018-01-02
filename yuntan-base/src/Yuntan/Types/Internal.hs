{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Yuntan.Types.Internal
  (
    Gateway (..)
  , initMgr
  , AppEnv (..)
  , Extra
  , CreatedAt
  ) where

import           Data.Aeson              (FromJSON (..), Value, withObject,
                                          (.!=), (.:), (.:?))
import           Data.Int                (Int64)
import           Data.String.Utils       (startswith)
import           Network.HTTP.Client     (Manager, defaultManagerSettings,
                                          managerConnCount,
                                          managerResponseTimeout, newManager,
                                          responseTimeoutMicro)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

data Gateway = Gateway { getGWUri        :: String
                       , getGWAppKey     :: String
                       , getGWAppSecret  :: String
                       , getGWNumThreads :: Int
                       , getGWTimeout    :: Int
                       , getGWConnCount  :: Int
                       , getGWMgr        :: Maybe Manager
                       -- numThreads of fetch async for haxl
                       }

instance Show Gateway where
  show a = concat [ "uri = ", getGWUri a
                  , ", key = ", getGWAppKey a
                  , ", secret = ", getGWAppSecret a
                  ]

initMgr :: Gateway -> IO Gateway
initMgr gw = do
  mgr <- newManager settings { managerConnCount = connCount
                             , managerResponseTimeout = responseTimeoutMicro $ timeout * 1000
                             }

  return gw { getGWMgr = Just mgr }

  where timeout = getGWTimeout gw
        connCount = getGWConnCount gw
        settings = if startswith "https" (getGWUri gw) then tlsManagerSettings
                                                       else defaultManagerSettings


instance FromJSON Gateway where
  parseJSON = withObject "Gateway" $ \o -> do
    getGWUri        <- o .:? "host"       .!= "https://gw.huabot.com"
    getGWAppKey     <- o .:  "key"
    getGWAppSecret  <- o .:  "secret"
    getGWNumThreads <- o .:  "numThreads" .!= 1
    getGWTimeout    <- o .:? "timeout"    .!= 30
    getGWConnCount  <- o .:? "conn-count" .!= 10
    return Gateway{ getGWMgr = Nothing, ..}

class AppEnv a where
  gateway :: a -> String -> Gateway

type Extra       = Value
type CreatedAt   = Int64
