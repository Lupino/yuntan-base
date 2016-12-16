module Dispatch.API.Coin
  (
    saveCoin
  , getCoinScore
  , getCoinList
  ) where

import           Data.Int                  (Int64)
import           Haxl.Core                 (dataFetch, uncachedRequest)

import           Dispatch.DS.Coin
import           Dispatch.Types.Coin
import           Dispatch.Types.Internal
import           Dispatch.Types.ListResult (From, ListResult, Size)
import           Dispatch.Types.Result     (ErrResult, OkResult)
import           Dispatch.Types.User       (UserName)

import           Haxl.Core                 (GenHaxl)

saveCoin         :: AppEnv u => UserName -> Coin -> GenHaxl u (Either ErrResult ScoreResult)
getCoinScore     :: AppEnv u => UserName -> GenHaxl u (Either ErrResult ScoreResult)
getCoinList      :: AppEnv u => UserName -> From -> Size -> GenHaxl u (ListResult Coin)

saveCoin n c          = uncachedRequest (SaveCoin n c)
getCoinScore n        = dataFetch (GetCoinScore n)
getCoinList n f si    = dataFetch (GetCoinList n f si)
