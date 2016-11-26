{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Dispatch.DataSource
  (
    DispatchReq (..)
  , initDispatchState
  ) where

import           Data.Hashable            (Hashable (..))
import           Data.Typeable            (Typeable)
import           Haxl.Core                (BlockedFetch (..), DataSource,
                                           DataSourceName, Flags,
                                           PerformFetch (..), Show1, State,
                                           StateKey, StateStore, dataSourceName,
                                           fetch, putFailure, putSuccess, show1,
                                           stateEmpty, stateSet)

import           Dispatch.DataSource.User
import           Dispatch.Types

import qualified Control.Exception        (SomeException, bracket_, try)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem

-- Data source implementation.

data DispatchReq a where
  CreateUser       :: UserName -> Password -> DispatchReq (Either ErrResult User)
  GetUser          :: UserName -> DispatchReq (Either ErrResult User)
  GetUsers         :: From -> Size -> DispatchReq (ListResult User)
  VerifyPasswd     :: UserName -> Password -> DispatchReq (Either ErrResult OkResult)
  RemoveUser       :: UserName -> DispatchReq (Either ErrResult OkResult)
  UpdateUserName   :: UserName -> UserName -> DispatchReq (Either ErrResult OkResult)
  UpdateUserPasswd :: UserName -> Password -> DispatchReq (Either ErrResult OkResult)
  UpdateUserExtra  :: UserName -> Extra -> DispatchReq (Either ErrResult OkResult)
  RemoveUserExtra  :: UserName -> Extra -> DispatchReq (Either ErrResult OkResult)
  ClearUserExtra   :: UserName -> DispatchReq (Either ErrResult OkResult)
  CreateBind       :: UserName -> Service -> ServiceName -> Extra -> DispatchReq (Either ErrResult Bind)
  GetBind          :: ServiceName -> DispatchReq (Either ErrResult Bind)
  DeleteBind       :: BindID -> DispatchReq (Either ErrResult OkResult)

  deriving (Typeable)

deriving instance Eq (DispatchReq a)
instance Hashable (DispatchReq a) where
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

deriving instance Show (DispatchReq a)
instance Show1 DispatchReq where show1 = show

instance StateKey DispatchReq where
  data State DispatchReq = DispatchState { numThreads :: Int }

instance DataSourceName DispatchReq where
  dataSourceName _ = "DispatchDataSource"

instance AppEnv u => DataSource u DispatchReq where
  fetch = dispatchFetch

dispatchFetch
  :: AppEnv u => State DispatchReq
  -> Flags
  -> u
  -> [BlockedFetch DispatchReq]
  -> PerformFetch

dispatchFetch _state _flags _user blockedFetches = AsyncFetch $ \inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) blockedFetches
  inner
  mapM_ wait asyncs

fetchAsync :: AppEnv u => QSem -> u -> BlockedFetch DispatchReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ fetchSync req gw

  where gw   = gateway env

fetchSync :: BlockedFetch DispatchReq -> Gateway -> IO ()
fetchSync (BlockedFetch req rvar) gw = do
  e <- Control.Exception.try $ fetchReq req gw
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: DispatchReq a -> Gateway -> IO a
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


initDispatchState :: Int -> StateStore
initDispatchState threads = stateSet dispatchState stateEmpty
  where dispatchState = DispatchState threads
