module Yuntan.API.User
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

import           Haxl.Core               (GenHaxl, dataFetch, uncachedRequest)

import           Yuntan.DS.User
import           Yuntan.Types.Internal
import           Yuntan.Types.ListResult (From, ListResult, Size)
import           Yuntan.Types.Result     (ErrResult, OkResult)
import           Yuntan.Types.User

createUser       :: AppEnv u => UserName -> Password -> GenHaxl u User
getUser          :: AppEnv u => UserName -> GenHaxl u User
getUsers         :: AppEnv u => From -> Size -> GenHaxl u (ListResult User)
verifyPasswd     :: AppEnv u => UserName -> Password -> GenHaxl u (OkResult String)
removeUser       :: AppEnv u => UserName -> GenHaxl u (OkResult String)
updateUserName   :: AppEnv u => UserName -> UserName -> GenHaxl u (OkResult String)
updateUserPasswd :: AppEnv u => UserName -> Password -> GenHaxl u (OkResult String)
updateUserExtra  :: AppEnv u => UserName -> Extra -> GenHaxl u (OkResult String)
removeUserExtra  :: AppEnv u => UserName -> Extra -> GenHaxl u (OkResult String)
clearUserExtra   :: AppEnv u => UserName -> GenHaxl u (OkResult String)
createBind       :: AppEnv u => UserName -> Service -> ServiceName -> Extra -> GenHaxl u Bind
getBind          :: AppEnv u => ServiceName -> GenHaxl u Bind
deleteBind       :: AppEnv u => BindID -> GenHaxl u (OkResult String)

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
