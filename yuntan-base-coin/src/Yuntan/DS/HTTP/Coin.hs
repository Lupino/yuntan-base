{-# LANGUAGE OverloadedStrings #-}

module Yuntan.DS.HTTP.Coin
  (
    saveCoin
  , getCoinScore
  , getCoinList
  , getCoinInfo
  , setCoinInfo
  ) where

import           Data.Aeson          (Value, encode)
import           Data.Aeson.Result   (From, List, Ok, Size)
import           Data.Text           (unpack)
import qualified Data.Text.Lazy      as LT (pack)
import           Network.Wreq
import           Network.Wreq.Helper
import           Yuntan.Base
import           Yuntan.Types.Coin

-- post "/api/coins/:name/"
saveCoin :: Name -> Coin -> Gateway opts -> IO (Ok Score)
saveCoin n c gw = do
  opts <- getOptionsAndSign "POST" path
    [ ("score", LT.pack $ show score)
    , ("type", LT.pack $ show tp)
    , ("desc", LT.pack $ unpack desc)
    , ("created_at", LT.pack $ show ct)
    ] gw
  responseOk_ "score" $ postWith opts uri
    [ "score" := score
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
getCoinScore :: Name -> Gateway opts -> IO (Ok Score)
getCoinScore n gw = do
  opts <- getOptionsAndSign "GET" path [] gw
  responseOk_ "score" $ getWith opts uri

  where path = "/api/coins/" ++ unpack n ++ "/score/"
        uri = host gw ++ path


-- get "/api/coins/:name/"
getCoinList :: Name -> From -> Size -> Gateway opts -> IO (List Coin)
getCoinList n f s gw = do
  opts <- getOptionsAndSign "GET" path
    [("from", LT.pack $ show f) , ("size", LT.pack $ show s)] gw
  responseList_ "coins" $ getWith opts uri

  where path = concat [ "/api/coins/", unpack n, "/"]
        uri = concat [ host gw, path, "?from=", show f, "&size=", show s]

-- get "/api/coins/:name/info/"
getCoinInfo :: Name -> Gateway opts -> IO CoinInfo
getCoinInfo n gw = do
  opts <- getOptionsAndSign "GET" path [] gw
  responseJSON $ getWith opts uri

  where path = concat [ "/api/coins/", unpack n, "/info/" ]
        uri = host gw ++ path

-- put "/api/coins/:name/info/"
setCoinInfo :: Name -> Value -> Gateway opts -> IO ()
setCoinInfo n v gw = do
  opts <- getOptionsAndSignJSON "PUT" path v gw
  eitherToError $ responseEither' $ putWith opts uri (encode v)

  where path = concat [ "/api/coins/", unpack n, "/info/" ]
        uri = host gw ++ path
