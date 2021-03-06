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

import           Haxl.Core             (GenHaxl, dataFetch, uncachedRequest)

import           Data.Aeson.Result     (From, List, Ok, Size)
import           Yuntan.DS.User
import           Yuntan.Types.Internal
import           Yuntan.Types.User

createUser       :: AppEnv u => UserName -> Password -> GenHaxl u w User
getUser          :: AppEnv u => UserName -> GenHaxl u w User
getUsers         :: AppEnv u => From -> Size -> GenHaxl u w (List User)
verifyPasswd     :: AppEnv u => UserName -> Password -> GenHaxl u w (Ok String)
removeUser       :: AppEnv u => UserName -> GenHaxl u w (Ok String)
updateUserName   :: AppEnv u => UserName -> UserName -> GenHaxl u w (Ok String)
updateUserPasswd :: AppEnv u => UserName -> Password -> GenHaxl u w (Ok String)
updateUserExtra  :: AppEnv u => UserName -> Extra -> GenHaxl u w (Ok String)
removeUserExtra  :: AppEnv u => UserName -> Extra -> GenHaxl u w (Ok String)
clearUserExtra   :: AppEnv u => UserName -> GenHaxl u w (Ok String)
createBind       :: AppEnv u => UserName -> Service -> ServiceName -> Extra -> GenHaxl u w Bind
getBind          :: AppEnv u => ServiceName -> GenHaxl u w Bind
deleteBind       :: AppEnv u => BindID -> GenHaxl u w (Ok String)

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
