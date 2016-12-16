{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Dispatch.DS.Coin
  (
    CoinReq (..)
  , initCoinState
  ) where

import           Data.Hashable             (Hashable (..))
import           Data.Typeable             (Typeable)
import           Haxl.Core                 (BlockedFetch (..), DataSource,
                                            DataSourceName, Flags,
                                            PerformFetch (..), Show1, State,
                                            StateKey, dataSourceName, fetch,
                                            putFailure, putSuccess, show1)

import           Dispatch.DS.HTTP.Coin
import           Dispatch.Types.Coin
import           Dispatch.Types.Internal
import           Dispatch.Types.ListResult (From, ListResult, Size)
import           Dispatch.Types.Result     (ErrResult, OkResult)
import           Dispatch.Types.User       (UserName)

import qualified Control.Exception         (SomeException, bracket_, try)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem

-- Data source implementation.

data CoinReq a where
  SaveCoin         :: UserName -> Coin -> CoinReq (Either ErrResult ScoreResult)
  GetCoinScore     :: UserName -> CoinReq (Either ErrResult ScoreResult)
  GetCoinList      :: UserName -> From -> Size -> CoinReq (ListResult Coin)

  deriving (Typeable)

deriving instance Eq (CoinReq a)
instance Hashable (CoinReq a) where
  hashWithSalt s (SaveCoin n c)       = hashWithSalt s (13::Int, n, c)
  hashWithSalt s (GetCoinScore n)     = hashWithSalt s (14::Int, n)
  hashWithSalt s (GetCoinList n f si) = hashWithSalt s (15::Int, n, f, si)



deriving instance Show (CoinReq a)
instance Show1 CoinReq where show1 = show

instance StateKey CoinReq where
  data State CoinReq = CoinState { numThreads :: Int }

instance DataSourceName CoinReq where
  dataSourceName _ = "CoinDataSource"

instance AppEnv u => DataSource u CoinReq where
  fetch = dispatchFetch

dispatchFetch
  :: AppEnv u => State CoinReq
  -> Flags
  -> u
  -> [BlockedFetch CoinReq]
  -> PerformFetch

dispatchFetch _state _flags _user blockedFetches = AsyncFetch $ \inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) blockedFetches
  inner
  mapM_ wait asyncs

fetchAsync :: AppEnv u => QSem -> u -> BlockedFetch CoinReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ fetchSync req gw

  where gw   = gateway env "CoinDataSource"

fetchSync :: BlockedFetch CoinReq -> Gateway -> IO ()
fetchSync (BlockedFetch req rvar) gw = do
  e <- Control.Exception.try $ fetchReq req gw
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: CoinReq a -> Gateway -> IO a
fetchReq (SaveCoin n c)       = saveCoin n c
fetchReq (GetCoinScore n)     = getCoinScore n
fetchReq (GetCoinList n f si) = getCoinList n f si


initCoinState :: Int -> State CoinReq
initCoinState threads = CoinState threads
