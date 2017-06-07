{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BSL
import           Data.Aeson (encode, object, (.=))
import           BlocMigrations
import           BuildArtifacts
import           Control.Error
import           Control.Lens ((^.), _1, _2)
import           Control.Monad
import           Data.Monoid
import           Control.Monad.IO.Class
import           System.Environment (lookupEnv)

main :: IO ()
main = do
  eRes <- runExceptT $ do
    admin <- getAdminFromEnv
    bloc <- makeBlocEnv
    yml <- lookupEnv "CONTRACTS_YAML" !? EnvError "Couldn't find CONTRACTS_YAML"
    liftIO $ print yml
    cDir <- lookupEnv "CONTRACTS_DIR" !? EnvError "Couldn't find CONTRACTS_DIR"
    liftIO $ print cDir
    buildRoot <- lookupEnv "BUILD_ROOT" !? EnvError "Couldn't find BUILD_ROOT"
    let buildDir = buildRoot <> "/build/contracts"
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
