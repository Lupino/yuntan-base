module Yuntan.API.Search
  (
    createIndex
  , getIndex
  , deleteIndex
  , listIndexes
  , docIndex
  , docCount
  , docGet
  , docDelete
  , search
  , listFields
  , debug
  , alias
  , initSearchState
  ) where

import           Data.Aeson            (Value)
import           Haxl.Core             (GenHaxl, dataFetch, uncachedRequest)

import           Yuntan.DS.Search
import           Yuntan.Types.Internal

createIndex :: AppEnv u => String -> Value -> GenHaxl u w Value
getIndex    :: AppEnv u => String -> GenHaxl u w Value
deleteIndex :: AppEnv u => String -> GenHaxl u w Value
listIndexes :: AppEnv u => GenHaxl u w Value
docIndex    :: AppEnv u => String -> String -> Value -> GenHaxl u w Value
docCount    :: AppEnv u => String -> GenHaxl u w Value
docGet      :: AppEnv u => String -> String -> GenHaxl u w Value
docDelete   :: AppEnv u => String -> String -> GenHaxl u w Value
search      :: AppEnv u => String -> Value -> GenHaxl u w Value
listFields  :: AppEnv u => String -> GenHaxl u w Value
debug       :: AppEnv u => String -> String -> GenHaxl u w Value
alias       :: AppEnv u => Value -> GenHaxl u w Value

createIndex n v = uncachedRequest (CreateIndex n v)
getIndex n      = dataFetch (GetIndex n)
deleteIndex n   = uncachedRequest (DeleteIndex n)
listIndexes     = dataFetch ListIndexes
docIndex n i v  = uncachedRequest (DocIndex n i v)
docCount n      = dataFetch (DocCount n)
docGet n i      = dataFetch (DocGet n i)
docDelete n i   = uncachedRequest (DocDelete n i)
search n v      = uncachedRequest (Search n v)
listFields n    = dataFetch (ListFields n)
debug n i       = uncachedRequest (Debug n i)
alias v         = uncachedRequest (Alias v)
