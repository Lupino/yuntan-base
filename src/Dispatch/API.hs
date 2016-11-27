module Dispatch.API
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

  , saveCoin
  , getCoinScore
  , getCoinList
  ) where

import           Data.Int            (Int64)
import           Haxl.Core           (dataFetch, uncachedRequest)

import           Dispatch.DataSource
import           Dispatch.Types

import           Haxl.Core           (GenHaxl)

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

saveCoin         :: AppEnv u => UserName -> Coin -> GenHaxl u (Either ErrResult ScoreResult)
getCoinScore     :: AppEnv u => UserName -> GenHaxl u (Either ErrResult ScoreResult)
getCoinList      :: AppEnv u => UserName -> From -> Size -> GenHaxl u (ListResult Coin)

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

saveCoin n c          = uncachedRequest (SaveCoin n c)
getCoinScore n        = dataFetch (GetCoinScore n)
getCoinList n f si    = dataFetch (GetCoinList n f si)
