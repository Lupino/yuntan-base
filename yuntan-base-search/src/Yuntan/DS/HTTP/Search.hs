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

import           Data.Aeson           (Value (Object, String), encode, object)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (insert)
import           Data.Text            (pack)
import           Network.Wreq
import           Network.Wreq.Helper  (responseJSON)
import           Yuntan.Base          (Gateway (host), getOptionsAndSignJSON)

insertPathName :: Value -> String -> Value
insertPathName (Object v') path = Object $ insert "pathname" (String $ pack path) v'
insertPathName _           _    = error "Unsupported"

commonRequest :: String -> (Options -> String -> ByteString -> IO (Response ByteString))
              -> String -> Value -> Gateway opts -> IO Value
commonRequest method req path v gw = do
  opts <- getOptionsAndSignJSON method path v gw
  responseJSON $ req opts uri (encode v)
  where uri = host gw ++ path

commonRequest_ :: String -> (Options -> String -> IO (Response ByteString))
              -> String -> Gateway opts -> IO Value
commonRequest_ method req path gw = do
  opts <- getOptionsAndSignJSON method path (object []) gw
  responseJSON $ req opts uri
  where uri = host gw ++ path

-- put "/api/:indexName"
createIndex :: String -> Value -> Gateway opts -> IO Value
createIndex indexName = commonRequest "PUT" putWith ("/api/" ++ indexName)

-- get "/api/:indexName"
getIndex :: String -> Gateway opts -> IO Value
getIndex indexName = commonRequest_ "GET" getWith ("/api/" ++ indexName)

-- delete "/api/:indexName"
deleteIndex :: String -> Gateway opts -> IO Value
deleteIndex indexName = commonRequest_ "DELETE" deleteWith ("/api/" ++ indexName)

-- get "/api"
listIndexes :: Gateway opts -> IO Value
listIndexes = commonRequest_ "GET" getWith "/api"

-- put "/api/:indexName/:docID"
docIndex :: String -> String -> Value -> Gateway opts -> IO Value
docIndex indexName docID = commonRequest "PUT" putWith path
  where path = concat ["/api/", indexName, "/", docID]

-- get "/api/:indexName/_count"
docCount :: String -> Gateway opts -> IO Value
docCount indexName = commonRequest_ "GET" getWith path
  where path = concat ["/api/", indexName, "/_count"]

-- get "/api/:indexName/:docID"
docGet :: String -> String -> Gateway opts -> IO Value
docGet indexName docID = commonRequest_ "GET" getWith path
  where path = concat ["/api/", indexName, "/", docID]

-- delete "/api/:indexName/:docID"
docDelete :: String -> String -> Gateway opts -> IO Value
docDelete indexName docID = commonRequest_ "GET" deleteWith path
  where path = concat ["/api/", indexName, "/", docID]

-- post "/api/:indexName/_search"
search :: String -> Value -> Gateway opts -> IO Value
search indexName = commonRequest "POST" postWith path
  where path = concat ["/api/", indexName, "/_search"]

-- get "/api/:indexName/_fields"
listFields :: String -> Gateway opts -> IO Value
listFields indexName = commonRequest_ "GET" getWith path
  where path = concat ["/api/", indexName, "/_fields"]

-- get "/api/:indexName/:docID/_debug"
debug :: String -> String -> Gateway opts -> IO Value
debug indexName docID = commonRequest_ "GET" getWith path
  where path = concat ["/api/", indexName, "/", docID, "/_debug"]

-- post "/api/_aliases"
alias :: Value -> Gateway opts -> IO Value
alias = commonRequest "POST" postWith path
  where path = "/api/_aliases"
