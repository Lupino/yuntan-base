{-# LANGUAGE OverloadedStrings #-}

module Dispatch.DataSource.HTTP.User
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

import           Data.Aeson     (encode)
import           Data.Text      (unpack)
import           Dispatch.Types
import           Dispatch.Utils
import           Network.Wreq

--   post   "/api/users/"
createUser :: UserName -> Password -> Gateway -> IO (Either ErrResult User)
createUser n p gw =
  responseEither $ asJSON =<< postWith opts uri [ "username" := t2b n
                                                , "passwd"   := t2b p
                                                ]

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/users/" ]

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
verifyPasswd n p gw =
  responseEither $ asJSON =<< postWith opts uri [ "passwd" := t2b p ]

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/users/", unpack n, "/verify" ]

--   delete "/api/users/:uidOrName/"
removeUser :: UserName -> Gateway -> IO (Either ErrResult OkResult)
removeUser n gw =
  responseEither $ asJSON =<< deleteWith opts uri

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/users/", unpack n, "/" ]

--   post   "/api/users/:uidOrName/"
updateUserName :: UserName -> UserName -> Gateway -> IO (Either ErrResult OkResult)
updateUserName n n1 gw =
  responseEither $ asJSON =<< postWith opts uri [ "username" := t2b n1 ]

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/users/", unpack n, "/" ]

--   post   "/api/users/:uidOrName/passwd"
updateUserPasswd :: UserName -> Password -> Gateway -> IO (Either ErrResult OkResult)
updateUserPasswd n p gw =
  responseEither $ asJSON =<< postWith opts uri [ "passwd" := t2b p ]

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/users/", unpack n, "/passwd" ]

--   post   "/api/users/:uidOrName/extra"
updateUserExtra :: UserName -> Extra -> Gateway -> IO (Either ErrResult OkResult)
updateUserExtra n ex gw =
  responseEither $ asJSON =<< postWith opts uri [ "extra" := encode ex ]

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/users/", unpack n, "/extra" ]

--   delete "/api/users/:uidOrName/extra"
removeUserExtra :: UserName -> Extra -> Gateway -> IO (Either ErrResult OkResult)
removeUserExtra n ex gw =
  responseEither $ asJSON =<< customPayloadMethodWith "DELETE" opts uri [ "extra" := encode ex ]

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/users/", unpack n, "/extra" ]

--   post   "/api/users/:uidOrName/extra/clear"
clearUserExtra :: UserName -> Gateway -> IO (Either ErrResult OkResult)
clearUserExtra n gw =
  responseEither $ asJSON =<< customMethodWith "POST" opts uri

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/users/", unpack n, "/extra/clear" ]

--   post   "/api/users/:uidOrName/binds"
createBind :: UserName -> Service -> ServiceName -> Extra -> Gateway -> IO (Either ErrResult Bind)
createBind n s sn ex gw =
  responseEither $ asJSON =<< postWith opts uri [ "service" := t2b s
                                                , "name"    := t2b sn
                                                , "extra"   := encode ex
                                                ]

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/users/", unpack n, "/binds" ]

--   get    "/api/binds/"
getBind :: ServiceName -> Gateway -> IO (Either ErrResult Bind)
getBind sn gw =
  responseEither $ asJSON =<< getWith opts uri

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/binds/?name=", unpack sn ]

--   delete "/api/binds/:bind_id"
deleteBind :: BindID -> Gateway -> IO (Either ErrResult OkResult)
deleteBind bid gw =
  responseEither $ asJSON =<< deleteWith opts uri

  where opts = getOptions gw
        uri = concat [ getGWUri gw, "/api/binds/", show bid ]
