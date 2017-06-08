{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BlocMigrations
import           BuildArtifacts
import           Control.Error
import           Control.Lens ((^.))
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
    artifacts <- forM (results^.migrationContractList) $ getContractAbis bloc buildDir
    liftIO $ forM_ (mconcat artifacts) writeArtifact
  case eRes of
    Left e -> print e
    Right () -> print ("Build Artifacts written!" :: String)
