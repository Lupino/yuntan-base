{-# LANGUAGE OverloadedStrings #-}

module Dispatch.DS.HTTP.User
  (
    createUser
  , getUser
  , getUsers
  , removeUser
  , verifyPasswd
  , updateUserName
  , updateUserPasswd
  , updateUserExtra
  , removeUserExtra
  , clearUserExtra
  , createBind
  , getBind
  , deleteBind
  ) where

import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as B (unpack)
import qualified Data.ByteString.Lazy      as LB (toStrict)
import           Data.Text                 (unpack)
import           Data.Text.Encoding        (encodeUtf8)
import qualified Data.Text.Lazy            as LT (fromStrict, pack)
import           Dispatch.Types.Internal
import           Dispatch.Types.ListResult (From, ListResult, Size)
import           Dispatch.Types.Result     (ErrResult, OkResult)
import           Dispatch.Types.User
import           Dispatch.Utils.Wreq
import           Network.Wreq

--   post   "/api/users/"
createUser :: UserName -> Password -> Gateway -> IO (Either ErrResult User)
createUser n p gw = do
  opts <- getOptionsAndSign [ ("username", LT.fromStrict n)
                            , ("passwd", LT.fromStrict p)
                            , ("sign_path", "/api/users/")
                            ] gw
  responseEither $ asJSON =<< postWith opts uri [ "username" := encodeUtf8 n
                                                , "passwd"   := encodeUtf8 p
                                                ]

  where uri = concat [ getGWUri gw, "/api/users/" ]

--   get    "/api/users/:uidOrName/"
getUser :: UserName -> Gateway -> IO (Either ErrResult User)
getUser n gw = do
  opts <- getOptionsAndSign [ ("sign_path", LT.pack $ path) ] gw
  responseEither $ asJSON =<< getWith opts uri

  where path = concat [ "/api/users/", unpack n, "/" ]
        uri = getGWUri gw ++ path

--   get    "/api/users/"
getUsers :: From -> Size -> Gateway -> IO (ListResult User)
getUsers f s gw = do
  opts <- getOptionsAndSign [ ("from", LT.pack $ show f)
                            , ("size", LT.pack $ show s)
                            , ("sign_path", "/api/users/")
                            ] gw
  responseListResult "users" $ asJSON =<< getWith opts uri

  where uri = concat [ getGWUri gw, "/api/users/?from=", show f, "&size=", show s]

--   post   "/api/users/:uidOrName/verify"
verifyPasswd :: UserName -> Password -> Gateway -> IO (Either ErrResult OkResult)
verifyPasswd n p gw = do
  opts <- getOptionsAndSign [ ("passwd", LT.fromStrict p)
                            , ("sign_path", LT.pack path)
                            ] gw
  responseEither $ asJSON =<< postWith opts uri [ "passwd" := encodeUtf8 p ]

  where path = concat [ "/api/users/", unpack n, "/verify" ]
        uri = getGWUri gw ++ path

--   delete "/api/users/:uidOrName/"
removeUser :: UserName -> Gateway -> IO (Either ErrResult OkResult)
removeUser n gw = do
  opts <- getOptionsAndSign [ ("sign_path", LT.pack path) ] gw
  responseEither $ asJSON =<< deleteWith opts uri

  where path = concat [ "/api/users/", unpack n, "/" ]
        uri = getGWUri gw ++ path

--   post   "/api/users/:uidOrName/"
updateUserName :: UserName -> UserName -> Gateway -> IO (Either ErrResult OkResult)
updateUserName n n1 gw = do
  opts <- getOptionsAndSign [ ("username", LT.fromStrict n1)
                            , ("sign_path", LT.pack path)
                            ] gw
  responseEither $ asJSON =<< postWith opts uri [ "username" := encodeUtf8 n1 ]

  where path = concat [ "/api/users/", unpack n, "/" ]
        uri = getGWUri gw ++ path

--   post   "/api/users/:uidOrName/passwd"
updateUserPasswd :: UserName -> Password -> Gateway -> IO (Either ErrResult OkResult)
updateUserPasswd n p gw = do
  opts <- getOptionsAndSign [ ("passwd", LT.fromStrict p)
                            , ("sign_path", LT.pack path)
                            ] gw
  responseEither $ asJSON =<< postWith opts uri [ "passwd" := encodeUtf8 p ]

  where path = concat [ "/api/users/", unpack n, "/passwd" ]
        uri = getGWUri gw ++ path

--   post   "/api/users/:uidOrName/extra"
updateUserExtra :: UserName -> Extra -> Gateway -> IO (Either ErrResult OkResult)
updateUserExtra n ex gw = do
  opts <- getOptionsAndSign [ ("extra", LT.pack $ B.unpack $ LB.toStrict ex')
                            , ("sign_path", LT.pack path)
                            ] gw
  responseEither $ asJSON =<< postWith opts uri [ "extra" := ex' ]

  where path = concat [ "/api/users/", unpack n, "/extra" ]
        uri = getGWUri gw ++ path
        ex' = encode ex

--   delete "/api/users/:uidOrName/extra"
removeUserExtra :: UserName -> Extra -> Gateway -> IO (Either ErrResult OkResult)
removeUserExtra n ex gw = do
  opts <- getOptionsAndSign [ ("extra", LT.pack $ B.unpack $ LB.toStrict ex')
                            , ("sign_path", LT.pack path)
                            ] gw
  responseEither $ asJSON =<< customPayloadMethodWith "DELETE" opts uri [ "extra" := ex' ]

  where path = concat [ "/api/users/", unpack n, "/extra" ]
        uri = getGWUri gw ++ path
        ex' = encode ex

--   post   "/api/users/:uidOrName/extra/clear"
clearUserExtra :: UserName -> Gateway -> IO (Either ErrResult OkResult)
clearUserExtra n gw = do
  opts <- getOptionsAndSign [ ("sign_path", LT.pack path) ] gw
  responseEither $ asJSON =<< customMethodWith "POST" opts uri

  where path = concat [ "/api/users/", unpack n, "/extra/clear" ]
        uri = getGWUri gw ++ path

--   post   "/api/users/:uidOrName/binds"
createBind :: UserName -> Service -> ServiceName -> Extra -> Gateway -> IO (Either ErrResult Bind)
createBind n s sn ex gw = do
  opts <- getOptionsAndSign [ ("service", LT.fromStrict s)
                            , ("name", LT.fromStrict sn)
                            , ("extra", LT.pack $ B.unpack $ LB.toStrict ex')
                            , ("sign_path", LT.pack path)
                            ] gw
  responseEither $ asJSON =<< postWith opts uri [ "service" := encodeUtf8 s
                                                , "name"    := encodeUtf8 sn
                                                , "extra"   := ex'
                                                ]

  where path = concat [ getGWUri gw, "/api/users/", unpack n, "/binds" ]
        uri = getGWUri gw ++ path
        ex' = encode ex

--   get    "/api/binds/"
getBind :: ServiceName -> Gateway -> IO (Either ErrResult Bind)
getBind sn gw = do
  opts <- getOptionsAndSign [ ("name", LT.fromStrict sn)
                            , ("sign_path", "/api/binds/")
                            ] gw
  responseEither $ asJSON =<< getWith opts uri

  where uri = concat [ getGWUri gw, "/api/binds/?name=", unpack sn ]

--   delete "/api/binds/:bind_id"
deleteBind :: BindID -> Gateway -> IO (Either ErrResult OkResult)
deleteBind bid gw = do
  opts <- getOptionsAndSign [ ("sign_path", LT.pack path) ] gw
  responseEither $ asJSON =<< deleteWith opts uri

  where path = concat [ "/api/binds/", show bid ]
        uri = getGWUri gw ++ path
