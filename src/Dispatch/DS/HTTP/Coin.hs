{-# LANGUAGE OverloadedStrings #-}

module Dispatch.DS.HTTP.Coin
  (
    saveCoin
  , getCoinScore
  , getCoinList
  , getCoinInfo
  , setCoinInfo
  ) where

import           Data.Aeson                (Value, encode, toJSON)
import           Data.Text                 (unpack)
import qualified Data.Text.Lazy            as LT (pack, unpack)
import           Dispatch.Types.Coin
import           Dispatch.Types.Internal
import           Dispatch.Types.ListResult (From, ListResult, Size)
import           Dispatch.Types.Result     (ErrResult, OkResult)
import           Dispatch.Types.User       (UserName)
import           Dispatch.Utils.Wreq
import           Network.Wreq

-- post "/api/coins/:name/"
saveCoin :: UserName -> Coin -> Gateway -> IO (Either ErrResult ScoreResult)
saveCoin n c gw = do
  opts <- getOptionsAndSign [ ("score", LT.pack $ show score)
                            , ("type", LT.pack $ show tp)
                            , ("desc", LT.pack $ unpack desc)
                            , ("created_at", LT.pack $ show ct)
                            ] gw
  responseEither $ asJSON =<< postWith opts uri [ "score" := score
                                                , "type" := show tp
                                                , "desc" := desc
                                                , "created_at" := ct
                                                ]

  where uri = concat [ getGWUri gw, "/api/coins/", unpack n, "/" ]

        score = coinScore c
        tp = coinType c
        desc = coinDesc c
        ct = coinCreatedAt c

-- get "/api/coins/:name/score/"
getCoinScore :: UserName -> Gateway -> IO (Either ErrResult ScoreResult)
getCoinScore n gw = do
  opts <- getOptionsAndSign [] gw
  responseEither $ asJSON =<< getWith opts uri

  where uri = concat [ getGWUri gw, "/api/coins/", unpack n, "/score/" ]

-- get "/api/coins/:name/"
getCoinList :: UserName -> From -> Size -> Gateway -> IO (ListResult Coin)
getCoinList n f s gw = do
  opts <- getOptionsAndSign [ ("from", LT.pack $ show f)
                            , ("size", LT.pack $ show s)
                            ] gw
  responseListResult "coins" $ asJSON =<< getWith opts uri

  where uri = concat [ getGWUri gw, "/api/coins/", unpack n, "/?from=", show f, "&size=", show s]

-- get "/api/coins/:name/info/"
getCoinInfo :: UserName -> Gateway -> IO (Either ErrResult CoinInfo)
getCoinInfo n gw = do
  opts <- getOptionsAndSign [] gw
  responseEither $ asJSON =<< getWith opts uri

  where uri = concat [ getGWUri gw, "/api/coins/", unpack n, "/info/" ]

-- put "/api/coins/:name/info/"
setCoinInfo :: UserName -> Value -> Gateway -> IO (Either ErrResult ())
setCoinInfo n v gw = do
  opts <- getOptionsAndSign' v gw
  responseEither' $ putWith opts uri (encode v)

  where uri = concat [ getGWUri gw, "/api/coins/", unpack n, "/info/" ]
