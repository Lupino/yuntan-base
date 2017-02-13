{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dispatch.Types.Coin
  (
    Coin (..)
  , CoinType (..)
  , zeroCoin
  , Score
  , ScoreResult (..)
  , CoinInfo (..)
  , coinInfo
  ) where

import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (..), object, withObject, (.!=),
                                          (.:), (.:?), (.=))
import           Data.Hashable           (Hashable (..))
import           Data.Int                (Int64)
import           Data.Text               (Text)
import           Dispatch.Types.Internal (CreatedAt)
import           GHC.Generics            (Generic)

type Score       = Int64

data CoinType = Incr | Decr deriving (Show, Read, Eq)

instance Hashable CoinType where
  hashWithSalt s Incr = hashWithSalt s (0::Int)
  hashWithSalt s Decr = hashWithSalt s (1::Int)

data Coin = Coin { coinType      :: CoinType
                 , coinScore     :: Score
                 , coinPreScore  :: Score
                 , coinDesc      :: Text
                 , coinCreatedAt :: CreatedAt
                 }
  deriving (Generic, Eq, Show)

instance Hashable Coin

instance FromJSON Coin where
  parseJSON = withObject "Coin" $ \o -> do
    coinType      <- read <$> o .: "type"
    coinScore     <- o .: "score"
    coinPreScore  <- o .: "pre_score"
    coinDesc      <- o .: "desc"
    coinCreatedAt <- o .: "created_at"
    return Coin{..}

instance ToJSON Coin where
  toJSON Coin{..} = object [ "type"       .= show coinType
                           , "score"      .= coinScore
                           , "pre_score"  .= coinPreScore
                           , "desc"       .= coinDesc
                           , "created_at" .= coinCreatedAt
                           ]

zeroCoin :: Coin
zeroCoin = Coin { coinType = Incr
                , coinScore = 0
                , coinPreScore = 0
                , coinDesc = ""
                , coinCreatedAt = 0
                }

data ScoreResult = ScoreResult { getScore :: Score }
  deriving (Show)

instance FromJSON ScoreResult where
  parseJSON = withObject "ScoreResult" $ \o -> do
    getScore <- o .: "score"
    return ScoreResult{..}

instance ToJSON ScoreResult where
  toJSON ScoreResult{..} = object [ "score" .= getScore ]

data CoinInfo = CoinInfo { infoName  :: String
                         , infoScore :: Score
                         , info      :: Value
                         }
  deriving (Show)

instance FromJSON CoinInfo where
  parseJSON = withObject "CoinInfo" $ \o -> do
    infoName  <- o .:  "name"
    infoScore <- o .:  "score"
    info      <- o .:? "info" .!= Null
    return CoinInfo{..}

instance ToJSON CoinInfo where
  toJSON CoinInfo{..} = object [ "score" .= infoScore
                               , "name"  .= infoName
                               , "info"  .= info
                               ]

coinInfo :: String -> CoinInfo
coinInfo infoName = CoinInfo { infoScore = 0, info = Null, .. }
