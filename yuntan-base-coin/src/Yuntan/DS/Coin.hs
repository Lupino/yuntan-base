{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Yuntan.DS.Coin
  (
    CoinReq (..)
  , initCoinState
  ) where

import           Data.Aeson               (Value)
import           Data.Hashable            (Hashable (..))
import           Data.Typeable            (Typeable)
import           Haxl.Core                (BlockedFetch (..), DataSource,
                                           DataSourceName, Flags,
                                           PerformFetch (..), ShowP, State,
                                           StateKey, dataSourceName, fetch,
                                           putFailure, putSuccess, showp)

import           Yuntan.DS.HTTP.Coin
import           Yuntan.Types.Coin
import           Yuntan.Types.Internal    hiding (numThreads)
import           Yuntan.Types.ListResult  (From, ListResult, Size)
import           Yuntan.Types.Result      (ErrResult, OkResult)

import qualified Control.Exception        (SomeException, bracket_, try)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem

-- Data source implementation.

data CoinReq a where
  SaveCoin         :: Name -> Coin -> CoinReq (OkResult Score)
  GetCoinScore     :: Name -> CoinReq (OkResult Score)
  GetCoinList      :: Name -> From -> Size -> CoinReq (ListResult Coin)
  GetCoinInfo      :: Name -> CoinReq CoinInfo
  SetCoinInfo      :: Name -> Value -> CoinReq ()

  deriving (Typeable)

deriving instance Eq (CoinReq a)
instance Hashable (CoinReq a) where
  hashWithSalt s (SaveCoin n c)       = hashWithSalt s (0::Int, n, c)
  hashWithSalt s (GetCoinScore n)     = hashWithSalt s (1::Int, n)
  hashWithSalt s (GetCoinList n f si) = hashWithSalt s (2::Int, n, f, si)
  hashWithSalt s (GetCoinInfo n)      = hashWithSalt s (3::Int, n)
  hashWithSalt s (SetCoinInfo n i)    = hashWithSalt s (4::Int, n, i)


deriving instance Show (CoinReq a)
instance ShowP CoinReq where showp = show

instance StateKey CoinReq where
  data State CoinReq = CoinState { numThreads :: Int }

instance DataSourceName CoinReq where
  dataSourceName _ = "CoinDataSource"

instance AppEnv u => DataSource u CoinReq where
  fetch = doFetch

doFetch
  :: AppEnv u => State CoinReq
  -> Flags
  -> u
  -> [BlockedFetch CoinReq]
  -> PerformFetch

doFetch _state _flags _user blockedFetches = AsyncFetch $ \inner -> do
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
fetchReq (GetCoinInfo n)      = getCoinInfo n
fetchReq (SetCoinInfo n i)    = setCoinInfo n i


initCoinState :: Int -> State CoinReq
initCoinState = CoinState
