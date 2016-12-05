{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dispatch.Types.Internal
  (
    Gateway (..)
  , AppEnv (..)
  , Extra
  , CreatedAt
  ) where

import           Data.Aeson (FromJSON (..), ToJSON (..), Value, object,
                             withObject, (.:), (.=))
import           Data.Int   (Int64)

data Gateway = Gateway { getGWUri       :: String
                       , getGWAppKey    :: String
                       , getGWAppSecret :: String
                       }
  deriving (Show)

class AppEnv a where
  gateway :: a -> String -> Gateway

type Extra       = Value
type CreatedAt   = Int64
