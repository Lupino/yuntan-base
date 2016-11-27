{-# LANGUAGE OverloadedStrings #-}

import           Dispatch
import           Haxl.Core     (GenHaxl, StateStore, initEnv, runHaxl,
                                stateEmpty, stateSet)

import           Control.Monad (void, when)
import           Data.Aeson    (Value (..), object, (.=))
import           Data.Text     (pack)

data UserEnv = UserEnv { getGateway :: Gateway }
  deriving (Show)

instance AppEnv UserEnv where
  gateway env "UserDataSource" = getGateway env

type DispatchM = GenHaxl UserEnv

type Test = DispatchM Bool

userEnv = UserEnv { getGateway = Gateway { getGWUri = "http://127.0.0.1:3300"
                                         , getGWAppKey = ""
                                         , getGWAppSecret = ""
                                         }
                  }

state = stateSet (initUserState 2) stateEmpty

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
  return $ length (getResult r) > 0

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
        ]

main :: IO ()
main = do
  env0 <- initEnv state userEnv
  ret <- mapM (runHaxl env0) tests
  print ret
