{-# LANGUAGE OverloadedStrings #-}

module Dispatch.DataSource.HTTP.Coin
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
  responseEither $ asJSON =<< postWith opts uri [ "score" := score
                                                , "type" := show tp
                                                , "desc" := desc
                                                , "created_at" := ct
                                                ]

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/coins/", unpack n, "/" ]

        score = coinScore c
        tp = coinType c
        desc = coinDesc c
        ct = coinCreatedAt c

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
