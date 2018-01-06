{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Yuntan.DS.Search
  (
    SearchReq (..)
  , initSearchState
  ) where

import           Data.Aeson               (Value)
import           Data.Hashable            (Hashable (..))
import           Data.Typeable            (Typeable)
import           Haxl.Core                (BlockedFetch (..), DataSource,
                                           DataSourceName, Flags,
                                           PerformFetch (..), ShowP, State,
                                           StateKey, dataSourceName, fetch,
                                           putFailure, putSuccess, showp)

import           Yuntan.DS.HTTP.Search
import           Yuntan.Types.Internal    hiding (numThreads)
import           Yuntan.Types.Result      (ErrResult)

import qualified Control.Exception        (SomeException, bracket_, try)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem

-- Data source implementation.

data SearchReq a where
  CreateIndex :: String -> Value -> SearchReq (Either ErrResult Value)
  GetIndex    :: String -> SearchReq (Either ErrResult Value)
  DeleteIndex :: String -> SearchReq (Either ErrResult Value)
  ListIndexes :: SearchReq (Either ErrResult Value)
  DocIndex    :: String -> String -> Value -> SearchReq (Either ErrResult Value)
  DocCount    :: String -> SearchReq (Either ErrResult Value)
  DocGet      :: String -> String -> SearchReq (Either ErrResult Value)
  DocDelete   :: String -> String -> SearchReq (Either ErrResult Value)
  Search      :: String -> Value -> SearchReq (Either ErrResult Value)
  ListFields  :: String -> SearchReq (Either ErrResult Value)
  Debug       :: String -> String -> SearchReq (Either ErrResult Value)
  Alias       :: Value -> SearchReq (Either ErrResult Value)

  deriving (Typeable)

deriving instance Eq (SearchReq a)
instance Hashable (SearchReq a) where
  hashWithSalt s (CreateIndex n v) = hashWithSalt s (0::Int, n, v)
  hashWithSalt s (GetIndex n)      = hashWithSalt s (1::Int, n)
  hashWithSalt s (DeleteIndex n)   = hashWithSalt s (2::Int, n)
  hashWithSalt s ListIndexes       = hashWithSalt s (3::Int)
  hashWithSalt s (DocIndex n i v)  = hashWithSalt s (4::Int, n, i, v)
  hashWithSalt s (DocCount n)      = hashWithSalt s (5::Int, n)
  hashWithSalt s (DocGet n i)      = hashWithSalt s (6::Int, n, i)
  hashWithSalt s (DocDelete n i)   = hashWithSalt s (7::Int, n, i)
  hashWithSalt s (Search n v)      = hashWithSalt s (8::Int, n, v)
  hashWithSalt s (ListFields n)    = hashWithSalt s (9::Int, n)
  hashWithSalt s (Debug n i)       = hashWithSalt s (10::Int, n, i)
  hashWithSalt s (Alias v)         = hashWithSalt s (11::Int, v)

deriving instance Show (SearchReq a)
instance ShowP SearchReq where showp = show

instance StateKey SearchReq where
  data State SearchReq = SearchState { numThreads :: Int }

instance DataSourceName SearchReq where
  dataSourceName _ = "SearchDataSource"

instance AppEnv u => DataSource u SearchReq where
  fetch = doFetch

doFetch
  :: AppEnv u => State SearchReq
  -> Flags
  -> u
  -> [BlockedFetch SearchReq]
  -> PerformFetch

doFetch _state _flags _user blockedFetches = AsyncFetch $ \inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) blockedFetches
  inner
  mapM_ wait asyncs

fetchAsync :: AppEnv u => QSem -> u -> BlockedFetch SearchReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ fetchSync req gw

  where gw   = gateway env "SearchDataSource"

fetchSync :: BlockedFetch SearchReq -> Gateway -> IO ()
fetchSync (BlockedFetch req rvar) gw = do
  e <- Control.Exception.try $ fetchReq req gw
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: SearchReq a -> Gateway -> IO a
fetchReq  (CreateIndex n v) = createIndex n v
fetchReq  (GetIndex n)      = getIndex n
fetchReq  (DeleteIndex n)   = deleteIndex n
fetchReq  ListIndexes       = listIndexes
fetchReq  (DocIndex n i v)  = docIndex n i v
fetchReq  (DocCount n)      = docCount n
fetchReq  (DocGet n i)      = docGet n i
fetchReq  (DocDelete n i)   = docDelete n i
fetchReq  (Search n v)      = search n v
fetchReq  (ListFields n)    = listFields n
fetchReq  (Debug n i)       = debug n i
fetchReq  (Alias v)         = alias v

initSearchState :: Int -> State SearchReq
initSearchState = SearchState
