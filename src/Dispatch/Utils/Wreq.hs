{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dispatch.Utils.Wreq
  (
    getOptions
  , getOptionsAndSign
  , getOptionsAndSign'
  , responseValue
  , responseMaybe
  , responseEither
  , responseEither'
  , responseListResult
  , tryResponse
  ) where

import           Control.Exception         (try)
import           Control.Lens              ((&), (.~), (^.), (^?))
import           Data.Aeson                (FromJSON (..), Result (..), ToJSON,
                                            Value (..), decode, fromJSON,
                                            toJSON)
import qualified Data.ByteString.Char8     as B (ByteString, empty, pack,
                                                 unpack)
import qualified Data.ByteString.Lazy      as LB (ByteString, fromStrict)
import           Data.HashMap.Strict       (insert)
import           Data.Text                 (Text, pack, unpack)
import qualified Data.Text.Lazy            as LT (Text, pack, unpack)
import           Data.UnixTime
import           Dispatch.Types.Internal   (Gateway (..))
import           Dispatch.Types.ListResult (ListResult, emptyListResult,
                                            toListResult)
import           Dispatch.Types.Result     (ErrResult, err)
import           Dispatch.Utils.Signature  (signJSON, signParams)
import           Network.HTTP.Client       (HttpException (..),
                                            HttpExceptionContent (..))
import           Network.HTTP.Client       (Manager)
import           Network.HTTP.Types        (ResponseHeaders)
import           Network.Wreq              (Options, Response, defaults, header,
                                            manager, responseBody)


getMgr :: Maybe Manager -> Options
getMgr Nothing    = defaults
getMgr (Just mgr) = defaults & manager .~ Right mgr

getOptions :: Gateway -> Options
getOptions (Gateway { getGWAppKey = key, getGWMgr = mgr }) =
  getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
             & header "User-Agent" .~ ["haskell dispatch-base-0.1.0.0"]

getOptionsAndSign :: [(LT.Text, LT.Text)] -> Gateway -> IO Options
getOptionsAndSign params (Gateway { getGWAppKey = key, getGWAppSecret = sec, getGWMgr = mgr }) = do
  t <- show . toEpochTime <$> getUnixTime
  let sign = signParams (B.pack sec) (("timestamp", LT.pack t):("key", LT.pack key):params)
      opts = getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
                        & header "X-REQUEST-SIGNATURE" .~ [sign]
                        & header "X-REQUEST-TIME" .~ [B.pack t]
                        & header "User-Agent" .~ ["haskell dispatch-base-0.1.0.0"]
  return opts

getOptionsAndSign' :: Value -> Gateway -> IO Options
getOptionsAndSign' (Object v) (Gateway { getGWAppKey = key, getGWAppSecret = sec, getGWMgr = mgr }) = do
  t <- show . toEpochTime <$> getUnixTime
  let v'   = insert "timestamp" (String $ pack t) $ insert "key" (String $ pack key) v
      sign = signJSON (B.pack sec) (Object v')
      opts = getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
                        & header "X-REQUEST-SIGNATURE" .~ [sign]
                        & header "X-REQUEST-TIME" .~ [B.pack t]
                        & header "User-Agent" .~ ["haskell dispatch-base-0.1.0.0"]
                        & header "Content-Type" .~ ["application/json"]
  return opts

responseValue :: IO (Response a) -> IO a
responseValue req = do
  r <- req
  return $ r ^. responseBody

responseMaybe :: IO (Response a) -> IO (Maybe a)
responseMaybe req = do
  e <- try req
  case e of
    Left (_ :: HttpException) -> return Nothing
    Right r                   -> return $ r ^? responseBody

headerResponseBody :: ResponseHeaders -> B.ByteString
headerResponseBody ((n, v):xs) | n == "X-Response-Body-Start" = v
                               | otherwise = headerResponseBody xs

headerResponseBody [] = B.empty

tryResponse :: IO (Response a) -> IO (Either ErrResult (Response a))
tryResponse req = do
  e <- try req
  case e of
    Left (HttpExceptionRequest _ content) -> do
      case content of
        (StatusCodeException _ body) -> do
          case decode . LB.fromStrict $ body of
            Just er -> return $ Left er
            Nothing -> return . Left . err . B.unpack $ body
        ResponseTimeout -> return . Left . err $ "ResponseTimeout"
        other -> return . Left . err $ show other

    Left (InvalidUrlException _ _) -> do
      return . Left . err $ "InvalidUrlException"
    Right r  -> return $ Right r

responseEither :: IO (Response a) -> IO (Either ErrResult a)
responseEither req = do
  rsp <- tryResponse req
  case rsp of
    Left e  -> return $ Left e
    Right r -> return . Right $ r ^. responseBody

responseEither' :: IO (Response LB.ByteString) -> IO (Either ErrResult ())
responseEither' req = do
  rsp <- tryResponse req
  case rsp of
    Left e  -> return $ Left e
    Right _ -> return $ Right ()

responseListResult :: FromJSON a => Text -> IO (Response Value) -> IO (ListResult a)
responseListResult okey req = do
  e <- try req
  case e of
    Left (_ :: HttpException) -> return emptyListResult
    Right r                   -> case toListResult okey (r ^. responseBody) of
                                   Just v  -> return v
                                   Nothing -> return emptyListResult
