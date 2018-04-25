{-# LANGUAGE OverloadedStrings #-}

module Yuntan.DS.HTTP.Coin
  (
    saveCoin
  , getCoinScore
  , getCoinList
  , getCoinInfo
  , setCoinInfo
  ) where

import           Data.Aeson              (Value, encode)
import           Data.Text               (unpack)
import qualified Data.Text.Lazy          as LT (pack)
import           Network.Wreq
import           Yuntan.Base
import           Yuntan.Types.Coin
import           Yuntan.Types.ListResult (From, ListResult, Size)
import           Yuntan.Types.Result     (OkResult)
import           Yuntan.Utils.Wreq

-- post "/api/coins/:name/"
saveCoin :: Name -> Coin -> Gateway -> IO (OkResult Score)
saveCoin n c gw = do
  opts <- getOptionsAndSign "POST" path
    [ ("score", LT.pack $ show score)
    , ("type", LT.pack $ show tp)
    , ("desc", LT.pack $ unpack desc)
    , ("created_at", LT.pack $ show ct)
    ] gw
  responseOkResult_ "score" $ postWith opts uri [ "score" := score
                                                , "type" := show tp
                                                , "desc" := desc
                                                , "created_at" := ct
                                                ]

  where path = "/api/coins/" ++ unpack n ++ "/"
        uri = host gw ++ path

        score = coinScore c
        tp = coinType c
        desc = coinDesc c
        ct = coinCreatedAt c

-- get "/api/coins/:name/score/"
getCoinScore :: Name -> Gateway -> IO (OkResult Score)
getCoinScore n gw = do
  opts <- getOptionsAndSign "GET" path [] gw
  responseOkResult_ "score" $ getWith opts uri

  where path = "/api/coins/" ++ unpack n ++ "/score/"
        uri = host gw ++ path


-- get "/api/coins/:name/"
getCoinList :: Name -> From -> Size -> Gateway -> IO (ListResult Coin)
getCoinList n f s gw = do
  opts <- getOptionsAndSign "GET" path
    [("from", LT.pack $ show f) , ("size", LT.pack $ show s)] gw
  responseListResult_ "coins" $ getWith opts uri

  where path = concat [ "/api/coins/", unpack n, "/"]
        uri = concat [ host gw, path, "?from=", show f, "&size=", show s]

-- get "/api/coins/:name/info/"
getCoinInfo :: Name -> Gateway -> IO CoinInfo
getCoinInfo n gw = do
  opts <- getOptionsAndSign "GET" path [] gw
  responseJSON $ getWith opts uri

  where path = concat [ "/api/coins/", unpack n, "/info/" ]
        uri = host gw ++ path

-- put "/api/coins/:name/info/"
setCoinInfo :: Name -> Value -> Gateway -> IO ()
setCoinInfo n v gw = do
  opts <- getOptionsAndSignJSON "PUT" path v gw
  eitherToError $ responseEither' $ putWith opts uri (encode v)

  where path = concat [ "/api/coins/", unpack n, "/info/" ]
        uri = host gw ++ path
