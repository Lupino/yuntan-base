module Dispatch.API.Coin
  (
    saveCoin
  , getCoinScore
  , getCoinList
  , getCoinInfo
  , setCoinInfo
  ) where

import           Data.Aeson                (Value)
import           Haxl.Core                 (dataFetch, uncachedRequest)

import           Dispatch.DS.Coin
import           Dispatch.Types.Coin
import           Dispatch.Types.Internal
import           Dispatch.Types.ListResult (From, ListResult, Size)
import           Dispatch.Types.Result     (ErrResult)
import           Dispatch.Types.User       (UserName)

import           Haxl.Core                 (GenHaxl)

saveCoin     :: AppEnv u => UserName -> Coin -> GenHaxl u (Either ErrResult ScoreResult)
getCoinScore :: AppEnv u => UserName -> GenHaxl u (Either ErrResult ScoreResult)
getCoinList  :: AppEnv u => UserName -> From -> Size -> GenHaxl u (ListResult Coin)
getCoinInfo  :: AppEnv u => UserName -> GenHaxl u (Either ErrResult CoinInfo)
setCoinInfo  :: AppEnv u => UserName -> Value -> GenHaxl u (Either ErrResult ())

saveCoin n c       = uncachedRequest (SaveCoin n c)
getCoinScore n     = dataFetch (GetCoinScore n)
getCoinList n f si = dataFetch (GetCoinList n f si)
getCoinInfo n      = dataFetch (GetCoinInfo n )
setCoinInfo n i    = uncachedRequest (SetCoinInfo n i)
