{-# LANGUAGE OverloadedStrings #-}

import           Haxl.Core               (GenHaxl, StateStore, initEnv, runHaxl,
                                          stateEmpty, stateSet)
import           Haxl.Core.Monad         (unsafeLiftIO)
import           Yuntan.Base
import           Yuntan.Types.ListResult

import           Control.Monad           (void, when)
import           Data.Aeson              (Value (..), object, (.=))
import           Data.Text               (pack)

data UserEnv = UserEnv { getGateway  :: Gateway -- for user
                       , getGateway1 :: Gateway -- for coin
                       }
  deriving (Show)

instance AppEnv UserEnv where
  gateway env "UserDataSource" = getGateway env
  gateway env "CoinDataSource" = getGateway1 env

type YuntanM = GenHaxl UserEnv

type Test = YuntanM Bool

userEnv = UserEnv { getGateway = Gateway { getGWUri = "http://127.0.0.1:3300"
                                         , getGWAppKey = "63dfdfbbdc5bc474b096"
                                         , getGWAppSecret = "68bb31f42bc29187badb7e182273769ba35a747dbe7c3925d9e23022fc746f"
                                         }
                  , getGateway1 = Gateway { getGWUri = "http://127.0.0.1:3300"
                                          , getGWAppKey = "611f6d62d7f0e0403319"
                                          , getGWAppSecret = "3341db8549272346f6aa3b4fd38ed413c1629df18a3e90bfe8851d87dd4747"
                                          }
                  }

state = stateSet (initCoinState 2) $ stateSet (initUserState 2) stateEmpty

testCreateUser :: Test
testCreateUser = do
  e <- createUser "Lupino" "passwd"
  case e of
    Left _  -> return False
    Right u -> return $ getUserName u == "Lupino"

testGetUser :: Test
testGetUser = do
  e <- getUser "Lupino"
  case e of
    Left _  -> return False
    Right u -> return $ getUserName u == "Lupino"

testGetUsers :: Test
testGetUsers = do
  r <- getUsers 0 10
  return . not $ null getResult r

testVerifyPasswd :: Test
testVerifyPasswd = do
  e <- verifyPasswd "Lupino" "passwd"
  case e of
    Left _  -> return False
    Right _ -> return True

testUpdateUserName :: Test
testUpdateUserName = do
  e <- updateUserName "Lupino" "Lupino2"
  case e of
    Left _  -> return False
    Right _ -> return True

testUpdateUserPasswd :: Test
testUpdateUserPasswd = do
  e <- updateUserPasswd "Lupino2" "passwd2"
  case e of
    Left _  -> return False
    Right _ -> return True

testUpdateUserExtra :: Test
testUpdateUserExtra = do
  e <- updateUserExtra "Lupino2" (object [ "test" .= pack "extra" ])
  case e of
    Left _  -> return False
    Right _ -> return True

testRemoveUserExtra :: Test
testRemoveUserExtra = do
  e <- removeUserExtra "Lupino2" (object [ "test" .= pack "extra" ])
  case e of
    Left _  -> return False
    Right _ -> return True

testClearUserExtra :: Test
testClearUserExtra = do
  e <- clearUserExtra "Lupino2"
  case e of
    Left _  -> return False
    Right _ -> return True

testCreateBind :: Test
testCreateBind = do
  e <- createBind "Lupino2" "qq" "qq-1016126595" Null
  case e of
    Left _  -> return False
    Right b -> return $ getBindName b == "qq-1016126595"

testGetBind :: Test
testGetBind = do
  e <- getBind "qq-1016126595"
  case e of
    Left _  -> return False
    Right b -> return $ getBindName b == "qq-1016126595"

testDeleteBind :: Test
testDeleteBind = do
  e <- getBind "qq-1016126595"
  case e of
    Left _  -> return False
    Right b -> do
      e1 <- deleteBind $ getBindID b
      case e1 of
        Left _  -> return False
        Right _ -> return True

testRemoveUser :: Test
testRemoveUser = do
  e <- removeUser "Lupino2"
  case e of
    Left _  -> return False
    Right _ -> return True

testSaveCoin :: Test
testSaveCoin = do
  e <- saveCoin "Lupino" zeroCoin { coinScore = 10 }
  unsafeLiftIO $ print e
  case e of
    Left _  -> return False
    Right _ -> return True

testGetScore :: Test
testGetScore = do
  e <- getCoinScore "Lupino"
  unsafeLiftIO $ print e
  case e of
    Left _  -> return False
    Right _ -> return True

testGetCoinList :: Test
testGetCoinList = do
  r <- getCoinList "Lupino" 0 10
  unsafeLiftIO $ print r
  return . not $ null getResult r

tests = [ testCreateUser
        , testGetUser
        , testGetUsers
        , testVerifyPasswd
        , testUpdateUserName
        , testUpdateUserPasswd
        , testUpdateUserExtra
        , testRemoveUserExtra
        , testClearUserExtra
        , testCreateBind
        , testGetBind
        , testDeleteBind
        , testRemoveUser
        , testSaveCoin
        , testGetScore
        , testGetCoinList
        ]

main :: IO ()
main = do
  env0 <- initEnv state userEnv
  ret <- mapM (runHaxl env0) tests
  print ret
