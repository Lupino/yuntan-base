{-# LANGUAGE OverloadedStrings #-}

module Yuntan.DS.HTTP.Search
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
  ) where

import           Data.Aeson            (Value (Object, String), encode, object,
                                        (.=))
import           Data.ByteString.Lazy  (ByteString)
import           Data.HashMap.Strict   (insert)
import           Data.Text             (pack)
import           Network.Wreq
import           Yuntan.Types.Internal (Gateway (getGWUri))
import           Yuntan.Types.Result   (ErrResult)
import           Yuntan.Utils.Wreq     (getOptionsAndSignJSON,
                                        responseEitherJSON)

insertPathName :: Value -> String -> Value
insertPathName (Object v') path = Object $ insert "pathname" (String $ pack path) v'
insertPathName _           _    = error "Unsupported"

commonRequest :: (Options -> String -> ByteString -> IO (Response ByteString))
              -> String -> Value -> Gateway -> IO (Either ErrResult Value)
commonRequest req path v gw = do
  opts <- getOptionsAndSignJSON (insertPathName v path) gw
  responseEitherJSON $ req opts uri (encode v)
  where uri = getGWUri gw ++ path

commonRequest_ :: (Options -> String -> IO (Response ByteString))
              -> String -> Gateway -> IO (Either ErrResult Value)
commonRequest_ req path gw = do
  opts <- getOptionsAndSignJSON (object [ "pathname" .= path ]) gw
  responseEitherJSON $ req opts uri
  where uri = getGWUri gw ++ path

-- put "/api/:indexName"
createIndex :: String -> Value -> Gateway -> IO (Either ErrResult Value)
createIndex indexName = commonRequest putWith ("/api/" ++ indexName)

-- get "/api/:indexName"
getIndex :: String -> Gateway -> IO (Either ErrResult Value)
getIndex indexName = commonRequest_ getWith ("/api/" ++ indexName)

-- delete "/api/:indexName"
deleteIndex :: String -> Gateway -> IO (Either ErrResult Value)
deleteIndex indexName = commonRequest_ deleteWith ("/api/" ++ indexName)

-- get "/api"
listIndexes :: Gateway -> IO (Either ErrResult Value)
listIndexes = commonRequest_ getWith "/api"

-- put "/api/:indexName/:docID"
docIndex :: String -> String -> Value -> Gateway -> IO (Either ErrResult Value)
docIndex indexName docID = commonRequest putWith path
  where path = concat ["/api/", indexName, "/", docID]

-- get "/api/:indexName/_count"
docCount :: String -> Gateway -> IO (Either ErrResult Value)
docCount indexName = commonRequest_ getWith path
  where path = concat ["/api/", indexName, "/_count"]

-- get "/api/:indexName/:docID"
docGet :: String -> String -> Gateway -> IO (Either ErrResult Value)
docGet indexName docID = commonRequest_ getWith path
  where path = concat ["/api/", indexName, "/", docID]

-- delete "/api/:indexName/:docID"
docDelete :: String -> String -> Gateway -> IO (Either ErrResult Value)
docDelete indexName docID = commonRequest_ deleteWith path
  where path = concat ["/api/", indexName, "/", docID]

-- post "/api/:indexName/_search"
search :: String -> Value -> Gateway -> IO (Either ErrResult Value)
search indexName = commonRequest postWith path
  where path = concat ["/api/", indexName, "/_search"]

-- get "/api/:indexName/_fields"
listFields :: String -> Gateway -> IO (Either ErrResult Value)
listFields indexName = commonRequest_ getWith path
  where path = concat ["/api/", indexName, "/_fields"]

-- get "/api/:indexName/:docID/_debug"
debug :: String -> String -> Gateway -> IO (Either ErrResult Value)
debug indexName docID = commonRequest_ getWith path
  where path = concat ["/api/", indexName, "/", docID, "/_debug"]

-- post "/api/_aliases"
alias :: Value -> Gateway -> IO (Either ErrResult Value)
alias = commonRequest postWith path
  where path = "/api/_aliases"
