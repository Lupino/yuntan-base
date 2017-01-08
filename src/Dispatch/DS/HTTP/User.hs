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
import qualified Data.Text.Lazy            as LT (pack, unpack)
import           Dispatch.Types.Internal
import           Dispatch.Types.ListResult (From, ListResult, Size)
import           Dispatch.Types.Result     (ErrResult, OkResult)
import           Dispatch.Types.User
import           Dispatch.Utils.Wreq
import           Network.Wreq

--   post   "/api/users/"
createUser :: UserName -> Password -> Gateway -> IO (Either ErrResult User)
createUser n p gw = do
  opts <- getOptionsAndSign [ ("username", LT.pack $ unpack n)
                            , ("passwd", LT.pack $ unpack p)
                            ] gw
  responseEither $ asJSON =<< postWith opts uri [ "username" := encodeUtf8 n
                                                , "passwd"   := encodeUtf8 p
                                                ]

  where uri = concat [ getGWUri gw, "/api/users/" ]

--   get    "/api/users/:uidOrName/"
getUser :: UserName -> Gateway -> IO (Either ErrResult User)
getUser n gw =
  responseEither $ asJSON =<< getWith opts uri

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/users/", unpack n, "/" ]

--   get    "/api/users/"
getUsers :: From -> Size -> Gateway -> IO (ListResult User)
getUsers f s gw = responseListResult "users" $ asJSON =<< getWith opts uri

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/users/?from=", show f, "&size=", show s]

--   post   "/api/users/:uidOrName/verify"
verifyPasswd :: UserName -> Password -> Gateway -> IO (Either ErrResult OkResult)
verifyPasswd n p gw = do
  opts <- getOptionsAndSign [ ("passwd", LT.pack $ unpack p) ] gw
  responseEither $ asJSON =<< postWith opts uri [ "passwd" := encodeUtf8 p ]

  where uri = concat [ getGWUri gw, "/api/users/", unpack n, "/verify" ]

--   delete "/api/users/:uidOrName/"
removeUser :: UserName -> Gateway -> IO (Either ErrResult OkResult)
removeUser n gw = do
  opts <- getOptionsAndSign [] gw
  responseEither $ asJSON =<< deleteWith opts uri

  where uri = concat [ getGWUri gw, "/api/users/", unpack n, "/" ]

--   post   "/api/users/:uidOrName/"
updateUserName :: UserName -> UserName -> Gateway -> IO (Either ErrResult OkResult)
updateUserName n n1 gw = do
  opts <- getOptionsAndSign [ ("username", LT.pack $ unpack n1) ] gw
  responseEither $ asJSON =<< postWith opts uri [ "username" := encodeUtf8 n1 ]

  where uri = concat [ getGWUri gw, "/api/users/", unpack n, "/" ]

--   post   "/api/users/:uidOrName/passwd"
updateUserPasswd :: UserName -> Password -> Gateway -> IO (Either ErrResult OkResult)
updateUserPasswd n p gw = do
  opts <- getOptionsAndSign [ ("passwd", LT.pack $ unpack p) ] gw
  responseEither $ asJSON =<< postWith opts uri [ "passwd" := encodeUtf8 p ]

  where uri = concat [ getGWUri gw, "/api/users/", unpack n, "/passwd" ]

--   post   "/api/users/:uidOrName/extra"
updateUserExtra :: UserName -> Extra -> Gateway -> IO (Either ErrResult OkResult)
updateUserExtra n ex gw = do
  opts <- getOptionsAndSign [ ("extra", LT.pack $ B.unpack $ LB.toStrict ex') ] gw
  responseEither $ asJSON =<< postWith opts uri [ "extra" := ex' ]

  where uri = concat [ getGWUri gw, "/api/users/", unpack n, "/extra" ]
        ex' = encode ex

--   delete "/api/users/:uidOrName/extra"
removeUserExtra :: UserName -> Extra -> Gateway -> IO (Either ErrResult OkResult)
removeUserExtra n ex gw = do
  opts <- getOptionsAndSign [ ("extra", LT.pack $ B.unpack $ LB.toStrict ex') ] gw
  responseEither $ asJSON =<< customPayloadMethodWith "DELETE" opts uri [ "extra" := ex' ]

  where uri = concat [ getGWUri gw, "/api/users/", unpack n, "/extra" ]
        ex' = encode ex

--   post   "/api/users/:uidOrName/extra/clear"
clearUserExtra :: UserName -> Gateway -> IO (Either ErrResult OkResult)
clearUserExtra n gw = do
  opts <- getOptionsAndSign [] gw
  responseEither $ asJSON =<< customMethodWith "POST" opts uri

  where uri = concat [ getGWUri gw, "/api/users/", unpack n, "/extra/clear" ]

--   post   "/api/users/:uidOrName/binds"
createBind :: UserName -> Service -> ServiceName -> Extra -> Gateway -> IO (Either ErrResult Bind)
createBind n s sn ex gw = do
  opts <- getOptionsAndSign [ ("service", LT.pack $ unpack s)
                            , ("name", LT.pack $ unpack sn)
                            , ("extra", LT.pack $ B.unpack $ LB.toStrict ex')
                            ] gw
  responseEither $ asJSON =<< postWith opts uri [ "service" := encodeUtf8 s
                                                , "name"    := encodeUtf8 sn
                                                , "extra"   := ex'
                                                ]

  where uri = concat [ getGWUri gw, "/api/users/", unpack n, "/binds" ]
        ex' = encode ex

--   get    "/api/binds/"
getBind :: ServiceName -> Gateway -> IO (Either ErrResult Bind)
getBind sn gw =
  responseEither $ asJSON =<< getWith opts uri

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/binds/?name=", unpack sn ]

--   delete "/api/binds/:bind_id"
deleteBind :: BindID -> Gateway -> IO (Either ErrResult OkResult)
deleteBind bid gw = do
  opts <- getOptionsAndSign [] gw
  responseEither $ asJSON =<< deleteWith opts uri

  where uri = concat [ getGWUri gw, "/api/binds/", show bid ]
