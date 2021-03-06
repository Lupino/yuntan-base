module Yuntan.API.Coin
  (
    saveCoin
  , getCoinScore
  , getCoinList
  , getCoinInfo
  , setCoinInfo
  , initCoinState
  ) where

import           Data.Aeson            (Value)
import           Haxl.Core             (GenHaxl, dataFetch, uncachedRequest)

import           Data.Aeson.Result     (From, List, Ok, Size)
import           Yuntan.DS.Coin
import           Yuntan.Types.Coin
import           Yuntan.Types.Internal

saveCoin     :: AppEnv u => Name -> Coin -> GenHaxl u w (Ok Score)
getCoinScore :: AppEnv u => Name -> GenHaxl u w (Ok Score)
getCoinList  :: AppEnv u => Name -> From -> Size -> GenHaxl u w (List Coin)
getCoinInfo  :: AppEnv u => Name -> GenHaxl u w CoinInfo
setCoinInfo  :: AppEnv u => Name -> Value -> GenHaxl u w ()

saveCoin n c       = uncachedRequest (SaveCoin n c)
getCoinScore n     = dataFetch (GetCoinScore n)
getCoinList n f si = dataFetch (GetCoinList n f si)
getCoinInfo n      = dataFetch (GetCoinInfo n )
setCoinInfo n i    = uncachedRequest (SetCoinInfo n i)
