{-# LANGUAGE OverloadedStrings #-}

module Yuntan.DS.HTTP.User
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

import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as LB (ByteString, toStrict)
import           Data.Text                  (unpack)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy             as LT (Text, fromStrict, pack)
import           Network.Wreq
import           Yuntan.Base
import           Yuntan.Types.ListResult    (From, ListResult, Size)
import           Yuntan.Types.Result        (ErrResult, OkResult)
import           Yuntan.Types.User
import           Yuntan.Utils.Wreq

b2t :: LB.ByteString -> LT.Text
b2t = LT.fromStrict . decodeUtf8 . LB.toStrict

--   post   "/api/users/"
createUser :: UserName -> Password -> Gateway -> IO (Either ErrResult User)
createUser n p gw = do
  opts <- getOptionsAndSign [ ("username", LT.fromStrict n)
                            , ("passwd", LT.fromStrict p)
                            , ("pathname", "/api/users/")
                            ] gw
  responseEitherJSON $ postWith opts uri [ "username" := encodeUtf8 n
                                         , "passwd"   := encodeUtf8 p
                                         ]

 where uri = host gw ++ "/api/users/"

--   get    "/api/users/:uidOrName/"
getUser :: UserName -> Gateway -> IO (Either ErrResult User)
getUser n gw = do
  opts <- getOptionsAndSign [ ("pathname", LT.pack path) ] gw
  responseEitherJSON $ getWith opts uri

  where path = concat [ "/api/users/", unpack n, "/" ]
        uri = host gw ++ path

--   get    "/api/users/"
getUsers :: From -> Size -> Gateway -> IO (ListResult User)
getUsers f s gw = do
  opts <- getOptionsAndSign [ ("from", LT.pack $ show f)
                            , ("size", LT.pack $ show s)
                            , ("pathname", "/api/users/")
                            ] gw
  responseListResult "users" $ getWith opts uri

  where uri = concat [ host gw, "/api/users/?from=", show f, "&size=", show s]

--   post   "/api/users/:uidOrName/verify"
verifyPasswd :: UserName -> Password -> Gateway -> IO (Either ErrResult (OkResult String))
verifyPasswd n p gw = do
  opts <- getOptionsAndSign [ ("passwd", LT.fromStrict p)
                            , ("pathname", LT.pack path)
                            ] gw
  responseEitherJSON $ postWith opts uri [ "passwd" := encodeUtf8 p ]

  where path = concat [ "/api/users/", unpack n, "/verify" ]
        uri = host gw ++ path

--   delete "/api/users/:uidOrName/"
removeUser :: UserName -> Gateway -> IO (Either ErrResult (OkResult String))
removeUser n gw = do
  opts <- getOptionsAndSign [ ("pathname", LT.pack path) ] gw
  responseEitherJSON $ deleteWith opts uri

  where path = concat [ "/api/users/", unpack n, "/" ]
        uri = host gw ++ path

--   post   "/api/users/:uidOrName/"
updateUserName :: UserName -> UserName -> Gateway -> IO (Either ErrResult (OkResult String))
updateUserName n n1 gw = do
  opts <- getOptionsAndSign [ ("username", LT.fromStrict n1)
                            , ("pathname", LT.pack path)
                            ] gw
  responseEitherJSON $ postWith opts uri [ "username" := encodeUtf8 n1 ]

  where path = concat [ "/api/users/", unpack n, "/" ]
        uri = host gw ++ path

--   post   "/api/users/:uidOrName/passwd"
updateUserPasswd :: UserName -> Password -> Gateway -> IO (Either ErrResult (OkResult String))
updateUserPasswd n p gw = do
  opts <- getOptionsAndSign [ ("passwd", LT.fromStrict p)
                            , ("pathname", LT.pack path)
                            ] gw
  responseEitherJSON $ postWith opts uri [ "passwd" := encodeUtf8 p ]

  where path = concat [ "/api/users/", unpack n, "/passwd" ]
        uri = host gw ++ path

--   post   "/api/users/:uidOrName/extra"
updateUserExtra :: UserName -> Extra -> Gateway -> IO (Either ErrResult (OkResult String))
updateUserExtra = userExtra "POST"

--   delete "/api/users/:uidOrName/extra"
removeUserExtra :: UserName -> Extra -> Gateway -> IO (Either ErrResult (OkResult String))
removeUserExtra = userExtra "DELETE"

userExtra :: String -> UserName -> Extra -> Gateway -> IO (Either ErrResult (OkResult String))
userExtra m n ex gw = do
  opts <- getOptionsAndSign [ ("extra", b2t ex')
                            , ("pathname", LT.pack path)
                            ] gw
  responseEitherJSON $ customPayloadMethodWith m opts uri [ "extra" := ex' ]

  where path = concat [ "/api/users/", unpack n, "/extra" ]
        uri = host gw ++ path
        ex' = encode ex

--   post   "/api/users/:uidOrName/extra/clear"
clearUserExtra :: UserName -> Gateway -> IO (Either ErrResult (OkResult String))
clearUserExtra n gw = do
  opts <- getOptionsAndSign [ ("pathname", LT.pack path) ] gw
  responseEitherJSON $ customMethodWith "POST" opts uri

  where path = concat [ "/api/users/", unpack n, "/extra/clear" ]
        uri = host gw ++ path

--   post   "/api/users/:uidOrName/binds"
createBind :: UserName -> Service -> ServiceName -> Extra -> Gateway -> IO (Either ErrResult Bind)
createBind n s sn ex gw = do
  opts <- getOptionsAndSign [ ("service", LT.fromStrict s)
                            , ("name", LT.fromStrict sn)
                            , ("extra", b2t ex')
                            , ("pathname", LT.pack path)
                            ] gw
  responseEitherJSON $ postWith opts uri [ "service" := encodeUtf8 s
                                         , "name"    := encodeUtf8 sn
                                         , "extra"   := ex'
                                         ]

  where path = concat [ "/api/users/", unpack n, "/binds" ]
        uri = host gw ++ path
        ex' = encode ex

--   get    "/api/binds/"
getBind :: ServiceName -> Gateway -> IO (Either ErrResult Bind)
getBind sn gw = do
  opts <- getOptionsAndSign [ ("name", LT.fromStrict sn)
                            , ("pathname", "/api/binds/")
                            ] gw
  responseEitherJSON $ getWith opts uri

  where uri = concat [ host gw, "/api/binds/?name=", unpack sn ]

--   delete "/api/binds/:bind_id"
deleteBind :: BindID -> Gateway -> IO (Either ErrResult (OkResult String))
deleteBind bid gw = do
  opts <- getOptionsAndSign [ ("pathname", LT.pack path) ] gw
  responseEitherJSON $ deleteWith opts uri

  where path = "/api/binds/" ++ show bid
        uri = host gw ++ path
