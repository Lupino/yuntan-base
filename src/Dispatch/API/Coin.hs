module Dispatch.API.Coin
  (
    saveCoin
  , getCoinScore
  , getCoinList
  , getCoinInfo
  , setCoinInfo
  , initCoinState
  ) where

import           Data.Aeson                (Value)
import           Haxl.Core                 (dataFetch, uncachedRequest)

import           Dispatch.DS.Coin
import           Dispatch.Types.Coin
import           Dispatch.Types.Internal
import           Dispatch.Types.ListResult (From, ListResult, Size)
import           Dispatch.Types.Result     (ErrResult)

import           Haxl.Core                 (GenHaxl)

saveCoin     :: AppEnv u => Name -> Coin -> GenHaxl u (Either ErrResult ScoreResult)
getCoinScore :: AppEnv u => Name -> GenHaxl u (Either ErrResult ScoreResult)
getCoinList  :: AppEnv u => Name -> From -> Size -> GenHaxl u (ListResult Coin)
getCoinInfo  :: AppEnv u => Name -> GenHaxl u (Either ErrResult CoinInfo)
setCoinInfo  :: AppEnv u => Name -> Value -> GenHaxl u (Either ErrResult ())

saveCoin n c       = uncachedRequest (SaveCoin n c)
getCoinScore n     = dataFetch (GetCoinScore n)
getCoinList n f si = dataFetch (GetCoinList n f si)
getCoinInfo n      = dataFetch (GetCoinInfo n )
setCoinInfo n i    = uncachedRequest (SetCoinInfo n i)
