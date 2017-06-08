{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BlocMigrations
import           BuildArtifacts
import           Control.Error
import           Control.Lens ((^.))
import           Control.Monad
import           Data.Monoid
import           Control.Monad.IO.Class
import           System.Directory (removePathForcibly)
import           System.Environment (lookupEnv)

main :: IO ()
main = do
  eRes <- runExceptT $ do
    admin <- getAdminFromEnv
    bloc <- makeBlocEnv
    yml <- lookupEnv "CONTRACTS_YAML" !? EnvError "Couldn't find CONTRACTS_YAML"
    cDir <- lookupEnv "CONTRACTS_DIR" !? EnvError "Couldn't find CONTRACTS_DIR"
    buildRoot <- lookupEnv "BUILD_ROOT" !? EnvError "Couldn't find BUILD_ROOT"
    let buildDir = buildRoot <> "/build"
    results <- ExceptT $ runMigration bloc admin yml cDir
    _ <- liftIO $ removePathForcibly buildDir
    liftIO . print $ ("Writing Admin Details" :: String)
    _ <- liftIO $ writeAdmin buildDir (results^.migrationAdminAddress)
    artifacts <- forM (results^.migrationContractList) $ getContractAbis bloc (buildDir <> "/contracts")
    liftIO $ forM_ (mconcat artifacts) writeArtifact
  case eRes of
    Left e -> putFailure e
    Right () -> putSuccess ("Build Artifacts written!" :: String)
