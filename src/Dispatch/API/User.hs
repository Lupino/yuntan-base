module Dispatch.API.User
  (
    createUser
  , getUser
  , getUsers
  , verifyPasswd
  , removeUser
  , updateUserName
  , updateUserPasswd
  , updateUserExtra
  , removeUserExtra
  , clearUserExtra
  , createBind
  , getBind
  , deleteBind
  , initUserState
  ) where

import           Haxl.Core                 (dataFetch, uncachedRequest)

import           Dispatch.DS.User
import           Dispatch.Types.Internal
import           Dispatch.Types.ListResult (From, ListResult, Size)
import           Dispatch.Types.Result     (ErrResult, OkResult)
import           Dispatch.Types.User

import           Haxl.Core                 (GenHaxl)

createUser       :: AppEnv u => UserName -> Password -> GenHaxl u (Either ErrResult User)
getUser          :: AppEnv u => UserName -> GenHaxl u (Either ErrResult User)
getUsers         :: AppEnv u => From -> Size -> GenHaxl u (ListResult User)
verifyPasswd     :: AppEnv u => UserName -> Password -> GenHaxl u (Either ErrResult OkResult)
removeUser       :: AppEnv u => UserName -> GenHaxl u (Either ErrResult OkResult)
updateUserName   :: AppEnv u => UserName -> UserName -> GenHaxl u (Either ErrResult OkResult)
updateUserPasswd :: AppEnv u => UserName -> Password -> GenHaxl u (Either ErrResult OkResult)
updateUserExtra  :: AppEnv u => UserName -> Extra -> GenHaxl u (Either ErrResult OkResult)
removeUserExtra  :: AppEnv u => UserName -> Extra -> GenHaxl u (Either ErrResult OkResult)
clearUserExtra   :: AppEnv u => UserName -> GenHaxl u (Either ErrResult OkResult)
createBind       :: AppEnv u => UserName -> Service -> ServiceName -> Extra -> GenHaxl u (Either ErrResult Bind)
getBind          :: AppEnv u => ServiceName -> GenHaxl u (Either ErrResult Bind)
deleteBind       :: AppEnv u => BindID -> GenHaxl u (Either ErrResult OkResult)

createUser n p        = uncachedRequest (CreateUser n p)
getUser n             = dataFetch (GetUser n)
getUsers f si         = dataFetch (GetUsers f si)
verifyPasswd n p      = uncachedRequest (VerifyPasswd n p)
removeUser n          = uncachedRequest (RemoveUser n)
updateUserName n n1   = uncachedRequest (UpdateUserName n n1)
updateUserPasswd n p  = uncachedRequest (UpdateUserPasswd n p)
updateUserExtra n ex  = uncachedRequest (UpdateUserExtra n ex)
removeUserExtra n ex  = uncachedRequest (RemoveUserExtra n ex)
clearUserExtra n      = uncachedRequest (ClearUserExtra n)
createBind n se sn ex = uncachedRequest (CreateBind n se sn ex)
getBind sn            = dataFetch (GetBind sn)
deleteBind bid        = uncachedRequest (DeleteBind bid)
