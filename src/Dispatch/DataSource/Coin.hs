{-# LANGUAGE OverloadedStrings #-}

module Dispatch.DataSource.Coin
  (
    saveCoin
  , getCoinScore
  , getCoinList
  ) where

import           Data.Aeson     (toJSON)
import           Data.Text      (unpack)
import           Dispatch.Types
import           Dispatch.Utils
import           Network.Wreq

-- post "/api/coins/:name/"
saveCoin :: UserName -> Coin -> Gateway -> IO (Either ErrResult ScoreResult)
saveCoin n c gw =
  responseEither $ asJSON =<< postWith opts uri (toJSON c)

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/coins/", unpack n, "/" ]

-- get "/api/coins/:name/score/"
getCoinScore :: UserName -> Gateway -> IO (Either ErrResult ScoreResult)
getCoinScore n gw =
  responseEither $ asJSON =<< getWith opts uri

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/coins/", unpack n, "/score/" ]

-- get "/api/coins/:name/"
getCoinList :: UserName -> From -> Size -> Gateway -> IO (ListResult Coin)
getCoinList n f s gw =
  responseListResult "coins" $ asJSON =<< getWith opts uri

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/coins/", unpack n, "/?from=", show f, "&size=", show s]
