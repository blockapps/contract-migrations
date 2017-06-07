{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL
import           Data.Aeson (encode, object, (.=))
import           BlocMigrations
import           BuildArtifacts
import           Control.Error
import           Control.Lens ((^.), _1, _2)
import           Control.Monad
import           System.Environment (lookupEnv)

main :: IO ()
main = do
  eRes <- runExceptT $ do
    admin <- getAdminFromEnv
    bloc <- makeBlocEnv
    yml <- lookupEnv "CONTRACTS_DIR" !? EnvError "Couldn't find CONTRACTS_YAML"
    cDir <- lookupEnv "CONTRACTS_DIR" !? EnvError "Couldn't find CONTRACTS_DIR"
    buildDir <- lookupEnv "BUILD_DIR" !? EnvError "Couldn't find BUILD_DIR"
    results <- ExceptT $ runMigration bloc admin yml cDir
    artifacts <- forM (results^.migrationContractList) $ getContractAbi bloc buildDir
    return (results^.migrationAdminAddress, buildDir, artifacts)
  case eRes of
    Left e -> print e
    Right (addr, buildDir, artifacts) -> do
      _ <- makeBuildDir buildDir
      _ <- BSL.writeFile "admin" . encode . object $ ["adminAddress" .= addr]
      _ <- forM_ artifacts $ \artifact -> BSL.writeFile (artifact ^. _1) (artifact^. _2)
      print ("Build Artifacts written!" :: String)
