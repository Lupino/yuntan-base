{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dispatch.Types.Internal
  (
    Gateway (..)
  , AppEnv (..)
  , Extra
  , CreatedAt
  ) where

import           Data.Aeson (FromJSON (..), Value, withObject, (.!=), (.:),
                             (.:?), (.=))
import           Data.Int   (Int64)

data Gateway = Gateway { getGWUri        :: String
                       , getGWAppKey     :: String
                       , getGWAppSecret  :: String
                       , getGWNumThreads :: Int
                       -- numThreads of fetch async for haxl
                       }
  deriving (Show)

instance FromJSON Gateway where
  parseJSON = withObject "Gateway" $ \o -> do
    getGWUri        <- o .:? "host"       .!= "https://gw.huabot.com"
    getGWAppKey     <- o .:  "key"
    getGWAppSecret  <- o .:  "secret"
    getGWNumThreads <- o .:  "numThreads" .!= 1
    return Gateway{..}

class AppEnv a where
  gateway :: a -> String -> Gateway

type Extra       = Value
type CreatedAt   = Int64
