{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Yuntan.Types.Internal
  (
    Gateway (..)
  , initMgr
  , AppEnv (..)
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

data Gateway = Gateway { host       :: String
                       , appKey     :: String
                       , appSecret  :: String
                       , numThreads :: Int
                       , timeout    :: Int
                       , connCount  :: Int
                       , mgr        :: Maybe Manager
                       -- numThreads of fetch async for haxl
                       }

instance Show Gateway where
  show a = concat [ "host = ", host a
                  , ", key = ", appKey a
                  , ", secret = ", appSecret a
                  ]

initMgr :: Gateway -> IO Gateway
initMgr gw@Gateway{..} = do
  mgr' <- newManager settings { managerConnCount = connCount
                              , managerResponseTimeout = responseTimeoutMicro $ timeout * 1000
                              }

  return gw { mgr = Just mgr' }

  where settings = if startswith "https" host then tlsManagerSettings
                                              else defaultManagerSettings


instance FromJSON Gateway where
  parseJSON = withObject "Gateway" $ \o -> do
    host       <- o .:? "host"       .!= "https://gw.huabot.com"
    appKey     <- o .:  "key"
    appSecret  <- o .:  "secret"
    numThreads <- o .:  "numThreads" .!= 1
    timeout    <- o .:? "timeout"    .!= 30
    connCount  <- o .:? "conn-count" .!= 10
    return Gateway{ mgr = Nothing, ..}

class AppEnv a where
  gateway :: a -> String -> Gateway
