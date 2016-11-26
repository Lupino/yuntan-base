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
  , replace
  ) where

import           Control.Exception     (try)
import           Control.Lens          ((&), (.~), (^.), (^?))
import           Data.Aeson            (FromJSON (..), Result (..), Value (..),
                                        decode, fromJSON)
import qualified Data.ByteString.Char8 as B (ByteString, empty, pack, unpack)
import qualified Data.ByteString.Lazy  as LB (ByteString, fromStrict)
import qualified Data.HashMap.Strict   as HM (delete, insert, lookupDefault)
import           Data.Text             (Text, unpack)
import           Dispatch.Types        (ErrResult (..), Gateway (..),
                                        ListResult (..), OkResult (..),
                                        emptyListResult)
import           Network.HTTP.Client   (HttpException (StatusCodeException))
import           Network.HTTP.Types    (ResponseHeaders)
import           Network.Wreq          (Options, Response, defaults, header,
                                        responseBody)


getOptions :: Gateway -> Options
getOptions (Gateway { getGWAppKey = key, getGWAppSecret = secret }) =
  defaults & header "X-APP-KEY" .~ [B.pack key] & header "X-APP-SECRET" .~ [B.pack secret]

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
                              | otherwise = mergeResponseBody xs

mergeResponseBody [] = B.empty

tryResponse :: IO (Response a) -> IO (Either ErrResult (Response a))
tryResponse req = do
  e <- try req
  case e of
    Left (StatusCodeException _ hdrs _) -> do
      case decode . LB.fromStrict $ headerResponseBody hdrs of
        Just err -> return $ Left err
        Nothing  -> return $ Left (ErrResult { errMsg = B.unpack $ headerResponseBody hdrs })
    Left _ -> return $ Left (ErrResult { errMsg = "unknow error" })
    Right r  -> return $ Right r

responseEither :: IO (Response a) -> IO (Either ErrResult a)
responseEither req = do
  rsp <- tryResponse req
  case rsp of
    Left e  -> return $ Left e
    Right r -> return . Right $ r ^. responseBody

replace :: Text -> Text -> Value -> Value
replace okey nkey (Object v) = Object . HM.delete okey $ HM.insert nkey ov v
  where ov = HM.lookupDefault Null okey v

responseListResult :: FromJSON a => Text -> IO (Response Value) -> IO (ListResult a)
responseListResult okey req = do
  e <- try req
  case e of
    Left (_ :: HttpException) -> return emptyListResult
    Right r                   -> case fromJSON (replace okey "result" $ r ^. responseBody) of
                                   Success v -> return v
                                   _         -> return emptyListResult
