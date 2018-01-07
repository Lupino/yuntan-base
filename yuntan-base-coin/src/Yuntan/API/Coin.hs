module Yuntan.API.Coin
  (
    saveCoin
  , getCoinScore
  , getCoinList
  , getCoinInfo
  , setCoinInfo
  , initCoinState
  ) where

import           Data.Aeson              (Value)
import           Haxl.Core               (GenHaxl, dataFetch, uncachedRequest)

import           Yuntan.DS.Coin
import           Yuntan.Types.Coin
import           Yuntan.Types.Internal
import           Yuntan.Types.ListResult (From, ListResult, Size)
import           Yuntan.Types.Result     (ErrResult, OkResult)

saveCoin     :: AppEnv u => Name -> Coin -> GenHaxl u (OkResult Score)
getCoinScore :: AppEnv u => Name -> GenHaxl u (OkResult Score)
getCoinList  :: AppEnv u => Name -> From -> Size -> GenHaxl u (ListResult Coin)
getCoinInfo  :: AppEnv u => Name -> GenHaxl u CoinInfo
setCoinInfo  :: AppEnv u => Name -> Value -> GenHaxl u ()

saveCoin n c       = uncachedRequest (SaveCoin n c)
getCoinScore n     = dataFetch (GetCoinScore n)
getCoinList n f si = dataFetch (GetCoinList n f si)
getCoinInfo n      = dataFetch (GetCoinInfo n )
setCoinInfo n i    = uncachedRequest (SetCoinInfo n i)
