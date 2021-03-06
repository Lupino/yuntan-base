{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Yuntan.DS.User
  (
    UserReq (..)
  , initUserState
  ) where

import           Data.Hashable            (Hashable (..))
import           Data.Typeable            (Typeable)
import           Haxl.Core                (BlockedFetch (..), DataSource,
                                           DataSourceName, Flags,
                                           PerformFetch (..), ShowP, State,
                                           StateKey, dataSourceName, fetch,
                                           putFailure, putSuccess, showp)

import           Data.Aeson.Result        (From, List, Ok, Size)
import           Yuntan.DS.HTTP.User
import           Yuntan.Types.Internal    hiding (numThreads)
import           Yuntan.Types.User

import qualified Control.Exception        (SomeException, bracket_, try)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem

-- Data source implementation.

data UserReq a where
  CreateUser       :: UserName -> Password -> UserReq User
  GetUser          :: UserName -> UserReq User
  GetUsers         :: From -> Size -> UserReq (List User)
  VerifyPasswd     :: UserName -> Password -> UserReq (Ok String)
  RemoveUser       :: UserName -> UserReq (Ok String)
  UpdateUserName   :: UserName -> UserName -> UserReq (Ok String)
  UpdateUserPasswd :: UserName -> Password -> UserReq (Ok String)
  UpdateUserExtra  :: UserName -> Extra -> UserReq (Ok String)
  RemoveUserExtra  :: UserName -> Extra -> UserReq (Ok String)
  ClearUserExtra   :: UserName -> UserReq (Ok String)
  CreateBind       :: UserName -> Service -> ServiceName -> Extra -> UserReq Bind
  GetBind          :: ServiceName -> UserReq Bind
  DeleteBind       :: BindID -> UserReq (Ok String)

  deriving (Typeable)

deriving instance Eq (UserReq a)
instance Hashable (UserReq a) where
  hashWithSalt s (CreateUser n p)        = hashWithSalt s (0::Int, n, p)
  hashWithSalt s (GetUser n)             = hashWithSalt s (1::Int, n)
  hashWithSalt s (GetUsers f si)         = hashWithSalt s (2::Int, f, si)
  hashWithSalt s (VerifyPasswd n p)      = hashWithSalt s (3::Int, n, p)
  hashWithSalt s (RemoveUser n)          = hashWithSalt s (4::Int, n)
  hashWithSalt s (UpdateUserName n n1)   = hashWithSalt s (5::Int, n, n1)
  hashWithSalt s (UpdateUserPasswd n p)  = hashWithSalt s (6::Int, n, p)
  hashWithSalt s (UpdateUserExtra n ex)  = hashWithSalt s (7::Int, n, ex)
  hashWithSalt s (RemoveUserExtra n ex)  = hashWithSalt s (8::Int, n, ex)
  hashWithSalt s (ClearUserExtra n)      = hashWithSalt s (9::Int, n)
  hashWithSalt s (CreateBind n se sn ex) = hashWithSalt s (10::Int, n, se, sn, ex)
  hashWithSalt s (GetBind sn)            = hashWithSalt s (11::Int, sn)
  hashWithSalt s (DeleteBind bid)        = hashWithSalt s (12::Int, bid)


deriving instance Show (UserReq a)
instance ShowP UserReq where showp = show

instance StateKey UserReq where
  data State UserReq = UserState { numThreads :: Int }

instance DataSourceName UserReq where
  dataSourceName _ = "UserDataSource"

instance AppEnv u => DataSource u UserReq where
  fetch = yuntanFetch

yuntanFetch
  :: AppEnv u => State UserReq
  -> Flags
  -> u
  -> PerformFetch UserReq

yuntanFetch _state _flags _user = AsyncFetch $ \reqs inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) reqs
  inner
  mapM_ wait asyncs

fetchAsync :: AppEnv u => QSem -> u -> BlockedFetch UserReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ fetchSync req gw

  where gw   = gateway env "UserDataSource"

fetchSync :: BlockedFetch UserReq -> Gateway opts -> IO ()
fetchSync (BlockedFetch req rvar) gw = do
  e <- Control.Exception.try $ fetchReq req gw
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: UserReq a -> Gateway opts -> IO a
fetchReq (CreateUser n p)        = createUser n p
fetchReq (GetUser n)             = getUser n
fetchReq (GetUsers f si)         = getUsers f si
fetchReq (VerifyPasswd n p)      = verifyPasswd n p
fetchReq (RemoveUser n)          = removeUser n
fetchReq (UpdateUserName n n1)   = updateUserName n n1
fetchReq (UpdateUserPasswd n p)  = updateUserPasswd n p
fetchReq (UpdateUserExtra n ex)  = updateUserExtra n ex
fetchReq (RemoveUserExtra n ex)  = removeUserExtra n ex
fetchReq (ClearUserExtra n)      = clearUserExtra n
fetchReq (CreateBind n se sn ex) = createBind n se sn ex
fetchReq (GetBind sn)            = getBind sn
fetchReq (DeleteBind bid)        = deleteBind bid


initUserState :: Int -> State UserReq
initUserState = UserState
