{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import           Network.HTTP.Client (defaultManagerSettings, newManager, managerModifyRequest)
import           Servant.Client
import           System.Environment


adminInfo ::  AdminConfig
adminInfo = AdminConfig "iam" "iam" "1"

blocClient :: IO ClientEnv
blocClient = do
  mgr <- newManager defaultManagerSettings {managerModifyRequest = \a -> print a >> return a}
  let url = BaseUrl Http "bayar4a.eastus.cloudapp.azure.com" 80 "/bloc"
  return $ ClientEnv mgr url


main :: IO ()
main = do
  [yml] <- getArgs
  bloc <- blocClient
  res <- runMigration bloc adminInfo yml "./contracts"
  print res
