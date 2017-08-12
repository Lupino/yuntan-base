{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yuntan.Utils.Wreq
  (
    getOptions
  , getOptionsAndSign
  , getOptionsAndSignJSON
  , getOptionsAndSignRaw
  , responseValue
  , responseMaybe
  , responseEither
  , responseEither'
  , responseEitherJSON
  , responseOkResult
  , responseListResult
  , tryResponse
  ) where

import           Control.Exception       (try)
import           Control.Lens            ((&), (.~), (^.), (^?))
import           Data.Aeson              (FromJSON (..), Value (..), decode)
import qualified Data.ByteString.Char8   as B (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy    as LB (ByteString, fromStrict)
import           Data.HashMap.Strict     (insert)
import           Data.Text               (Text, pack)
import qualified Data.Text.Lazy          as LT (Text, pack)
import           Data.UnixTime
import           Network.HTTP.Client     (HttpException (..),
                                          HttpExceptionContent (..), Manager)
import           Network.Wreq            (Options, Response, asJSON, defaults,
                                          header, manager, responseBody)
import           Yuntan.Types.Internal   (Gateway (..))
import           Yuntan.Types.ListResult (ListResult, emptyListResult,
                                          toListResult)
import           Yuntan.Types.Result     (ErrResult, OkResult, err, toOkResult)
import           Yuntan.Utils.Signature  (signJSON, signParams, signRaw)


getMgr :: Maybe Manager -> Options
getMgr Nothing    = defaults
getMgr (Just mgr) = defaults & manager .~ Right mgr

getOptions :: Gateway -> Options
getOptions (Gateway { getGWAppKey = key, getGWMgr = mgr }) =
  getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
             & header "User-Agent" .~ ["haskell yuntan-base-0.1.0.0"]

getOptionsAndSign :: [(LT.Text, LT.Text)] -> Gateway -> IO Options
getOptionsAndSign params (Gateway { getGWAppKey = key, getGWAppSecret = sec, getGWMgr = mgr }) = do
  t <- show . toEpochTime <$> getUnixTime
  let sign = signParams (B.pack sec) (("timestamp", LT.pack t):("key", LT.pack key):params)
      opts = getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
                        & header "X-REQUEST-SIGNATURE" .~ [sign]
                        & header "X-REQUEST-TIME" .~ [B.pack t]
                        & header "User-Agent" .~ ["haskell yuntan-base-0.1.0.0"]
  return opts

getOptionsAndSignJSON :: Value -> Gateway -> IO Options
getOptionsAndSignJSON (Object v) (Gateway { getGWAppKey = key, getGWAppSecret = sec, getGWMgr = mgr }) = do
  t <- show . toEpochTime <$> getUnixTime
  let v'   = insert "timestamp" (String $ pack t) $ insert "key" (String $ pack key) v
      sign = signJSON (B.pack sec) (Object v')
      opts = getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
                        & header "X-REQUEST-SIGNATURE" .~ [sign]
                        & header "X-REQUEST-TIME" .~ [B.pack t]
                        & header "User-Agent" .~ ["haskell yuntan-base-0.1.0.0"]
                        & header "Content-Type" .~ ["application/json"]
  return opts

getOptionsAndSignJSON (Array _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON (String _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON (Number _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON (Bool _) _ = error "Unsupport Aeson.Value signature"
getOptionsAndSignJSON Null _ = error "Unsupport Aeson.Value signature"

getOptionsAndSignRaw :: String -> B.ByteString -> Gateway -> IO Options
getOptionsAndSignRaw path dat (Gateway { getGWAppKey = key, getGWAppSecret = sec, getGWMgr = mgr }) = do
  t <- show . toEpochTime <$> getUnixTime
  let sign = signRaw (B.pack sec) [ ("key", B.pack key)
                                  , ("timestamp", B.pack t)
                                  , ("raw", dat)
                                  , ("sign_path", B.pack path)
                                  ]
      opts = getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
                        & header "X-REQUEST-SIGNATURE" .~ [sign]
                        & header "X-REQUEST-TIME" .~ [B.pack t]
                        & header "User-Agent" .~ ["haskell yuntan-base-0.1.0.0"]
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

responseEitherJSON :: FromJSON a => IO (Response LB.ByteString) -> IO (Either ErrResult a)
responseEitherJSON req = responseEither $ asJSON =<< req

responseOkResult :: FromJSON a => Text -> IO (Response LB.ByteString) -> IO (Either ErrResult (OkResult a))
responseOkResult okey req = do
  rsp <- responseEitherJSON req
  case rsp of
    Left e  -> return $ Left e
    Right r -> case toOkResult okey r of
                 Just v  -> return $ Right v
                 Nothing -> return . Left $ err "Invalid Result"

responseListResult :: FromJSON a => Text -> IO (Response LB.ByteString) -> IO (ListResult a)
responseListResult okey req = do
  e <- try $ asJSON =<< req
  case e of
    Left (_ :: HttpException) -> return emptyListResult
    Right r                   -> case toListResult okey (r ^. responseBody) of
                                   Just v  -> return v
                                   Nothing -> return emptyListResult
