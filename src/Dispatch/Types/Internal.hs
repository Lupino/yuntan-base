{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dispatch.Types.Internal
  (
    Gateway (..)
  , AppEnv (..)
  , From
  , Size
  , Total
  , Extra
  , CreatedAt
  , ListResult (..)
  , ErrResult (..)
  , OkResult (..)
  , emptyListResult
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
  gateway :: a -> Gateway

type From        = Int64
type Size        = Int64
type Total       = Int64
type Extra       = Value
type CreatedAt   = Int64

data ListResult a = ListResult { getFrom   :: From
                               , getSize   :: Size
                               , getTotal  :: Total
                               , getResult :: [a]
                               }
  deriving (Show)


instance FromJSON a => FromJSON (ListResult a) where
  parseJSON = withObject "ListResult" $ \o -> do
    getFrom   <- o .: "from"
    getSize   <- o .: "size"
    getTotal  <- o .: "total"
    getResult <- o .: "result"
    return ListResult{..}

instance ToJSON a => ToJSON (ListResult a) where
  toJSON ListResult{..} = object [ "from"   .= getFrom
                                 , "size"   .= getSize
                                 , "total"  .= getTotal
                                 , "result" .= getResult
                                 ]

emptyListResult :: ListResult a
emptyListResult = ListResult { getFrom = 0
                             , getSize = 10
                             , getTotal = 0
                             , getResult = []
                             }

data OkResult = OkResult { okMsg :: String }
  deriving (Show)

instance FromJSON OkResult where
  parseJSON = withObject "OkResult" $ \o -> do
    okMsg <- o .: "result"
    return OkResult{..}

instance ToJSON OkResult where
  toJSON OkResult{..} = object [ "result"   .= okMsg ]

data ErrResult = ErrResult { errMsg :: String }
  deriving (Show)

instance FromJSON ErrResult where
  parseJSON = withObject "ErrResult" $ \o -> do
    errMsg <- o .: "err"
    return ErrResult{..}

instance ToJSON ErrResult where
  toJSON ErrResult{..} = object [ "err"   .= errMsg ]
