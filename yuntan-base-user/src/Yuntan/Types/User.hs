{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Yuntan.Types.User
  (
    User (..)
  , Bind (..)
  , UserID
  , BindID
  , UserName
  , Password
  , Service
  , ServiceName
  , Extra
  , CreatedAt
  ) where

import           Data.Aeson (FromJSON (..), ToJSON (..), Value, object,
                             withObject, (.:), (.=))
import           Data.Int   (Int64)
import           Data.Text  (Text)

type UserID      = Int64
type BindID      = Int64
type UserName    = Text
type Password    = Text
type Service     = Text
type ServiceName = Text
type CreatedAt   = Int64
type Extra       = Value

data User = User { getUserID        :: UserID
                 , getUserName      :: UserName
                 , getUserExtra     :: Extra
                 , getUserBinds     :: [Bind]
                 , getUserCreatedAt :: CreatedAt
                 }
  deriving (Show)

data Bind = Bind { getBindID        :: BindID
                 , getBindUid       :: UserID
                 , getBindService   :: Service
                 , getBindName      :: ServiceName
                 , getBindExtra     :: Extra
                 , getBindCreatedAt :: CreatedAt
                 }
  deriving (Show)

instance ToJSON User where
  toJSON User{..} = object [ "id"         .= getUserID
                           , "name"       .= getUserName
                           , "extra"      .= getUserExtra
                           , "binds"      .= getUserBinds
                           , "created_at" .= getUserCreatedAt
                           ]

instance ToJSON Bind where
  toJSON Bind{..} = object [ "id"         .= getBindID
                           , "user_id"    .= getBindUid
                           , "name"       .= getBindName
                           , "service"    .= getBindService
                           , "extra"      .= getBindExtra
                           , "created_at" .= getBindCreatedAt
                           ]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    getUserID        <- o .: "id"
    getUserName      <- o .: "name"
    getUserExtra     <- o .: "extra"
    getUserBinds     <- o .: "binds"
    getUserCreatedAt <- o .: "created_at"
    return User{..}

instance FromJSON Bind where
  parseJSON = withObject "Bind" $ \o -> do
    getBindID        <- o .: "id"
    getBindUid       <- o .: "user_id"
    getBindName      <- o .: "name"
    getBindService   <- o .: "service"
    getBindExtra     <- o .: "extra"
    getBindCreatedAt <- o .: "created_at"
    return Bind{..}
