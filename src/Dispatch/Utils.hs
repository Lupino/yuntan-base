{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dispatch.Utils
  (
    getOptions
  , t2b
  , responseValue
  , responseMaybe
  , responseEither
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
import           Data.Text                 (Text, unpack)
import           Dispatch.Types            (Gateway (..))
import           Dispatch.Types.ListResult (ListResult, emptyListResult,
                                            toListResult)
import           Dispatch.Types.Result     (ErrResult, err)
import           Network.HTTP.Client       (HttpException (StatusCodeException))
import           Network.HTTP.Types        (ResponseHeaders)
import           Network.Wreq              (Options, Response, defaults, header,
                                            responseBody)


getOptions :: Gateway -> Options
getOptions (Gateway { getGWAppKey = key, getGWAppSecret = secret }) =
  defaults & header "X-APP-KEY" .~ [B.pack key]
           & header "X-APP-SECRET" .~ [B.pack secret]
           & header "User-Agent" .~ ["haskell dispatch-base-0.1.0.0"]

t2b :: Text -> B.ByteString
t2b = B.pack . unpack

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
    Left (StatusCodeException _ hdrs _) -> do
      case decode . LB.fromStrict $ headerResponseBody hdrs of
        Just er -> return $ Left er
        Nothing -> return . Left . err . B.unpack $ headerResponseBody hdrs

    Left _ -> return . Left $ err "unknow error"
    Right r  -> return $ Right r

responseEither :: IO (Response a) -> IO (Either ErrResult a)
responseEither req = do
  rsp <- tryResponse req
  case rsp of
    Left e  -> return $ Left e
    Right r -> return . Right $ r ^. responseBody

responseListResult :: FromJSON a => Text -> IO (Response Value) -> IO (ListResult a)
responseListResult okey req = do
  e <- try req
  case e of
    Left (_ :: HttpException) -> return emptyListResult
    Right r                   -> case toListResult okey (r ^. responseBody) of
                                   Just v  -> return v
                                   Nothing -> return emptyListResult
