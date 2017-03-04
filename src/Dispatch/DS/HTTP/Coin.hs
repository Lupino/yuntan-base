{-# LANGUAGE OverloadedStrings #-}

module Dispatch.DS.HTTP.Coin
  (
    saveCoin
  , getCoinScore
  , getCoinList
  , getCoinInfo
  , setCoinInfo
  ) where

import           Data.Aeson                (Value (Object, String), encode)
import           Data.HashMap.Strict       (insert)
import           Data.Text                 (pack, unpack)
import qualified Data.Text.Lazy            as LT (pack)
import           Dispatch.Types.Coin
import           Dispatch.Types.Internal
import           Dispatch.Types.ListResult (From, ListResult, Size)
import           Dispatch.Types.Result     (ErrResult)
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
                            , ("sign_path", LT.pack path)
                            ] gw
  responseEither $ asJSON =<< postWith opts uri [ "score" := score
                                                , "type" := show tp
                                                , "desc" := desc
                                                , "created_at" := ct
                                                ]

  where path = "/api/coins/" ++ unpack n ++ "/"
        uri = getGWUri gw ++ path

        score = coinScore c
        tp = coinType c
        desc = coinDesc c
        ct = coinCreatedAt c

-- get "/api/coins/:name/score/"
getCoinScore :: UserName -> Gateway -> IO (Either ErrResult ScoreResult)
getCoinScore n gw = do
  opts <- getOptionsAndSign [ ("sign_path", LT.pack path) ] gw
  responseEither $ asJSON =<< getWith opts uri

  where path = "/api/coins/" ++ unpack n ++ "/score/"
        uri = getGWUri gw ++ path


-- get "/api/coins/:name/"
getCoinList :: UserName -> From -> Size -> Gateway -> IO (ListResult Coin)
getCoinList n f s gw = do
  opts <- getOptionsAndSign [ ("from", LT.pack $ show f)
                            , ("size", LT.pack $ show s)
                            , ("sign_path", LT.pack path)
                            ] gw
  responseListResult "coins" $ asJSON =<< getWith opts uri

  where path = concat [ "/api/coins/", unpack n, "/"]
        uri = concat [ getGWUri gw, path, "?from=", show f, "&size=", show s]

-- get "/api/coins/:name/info/"
getCoinInfo :: UserName -> Gateway -> IO (Either ErrResult CoinInfo)
getCoinInfo n gw = do
  opts <- getOptionsAndSign [ ("sign_path", LT.pack path) ] gw
  responseEither $ asJSON =<< getWith opts uri

  where path = concat [ "/api/coins/", unpack n, "/info/" ]
        uri = getGWUri gw ++ path

-- put "/api/coins/:name/info/"
setCoinInfo :: UserName -> Value -> Gateway -> IO (Either ErrResult ())
setCoinInfo n v gw = do
  opts <- getOptionsAndSign' (params v) gw
  responseEither' $ putWith opts uri (encode v)

  where path = concat [ "/api/coins/", unpack n, "/info/" ]
        uri = getGWUri gw ++ path

        params :: Value -> Value
        params (Object v') = Object $ insert "sign_path" (String $ pack path) v'
        params _           = error "Unsupport coin info"
