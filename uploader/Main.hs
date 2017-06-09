{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BlocMigrations
import           BuildArtifacts
import qualified Control.Error as E
import           Control.Lens ((^.))
import           Control.Monad
import           Data.Monoid
import           Control.Monad.IO.Class
import           System.Directory (removePathForcibly)
import           System.Environment (lookupEnv)

main :: IO ()
main = do
  eRes <- E.runExceptT $ do
    cfg <- mkMigrationConfig
    buildRoot <- lookupEnv "BUILD_ROOT" E.!? EnvError "Couldn't find BUILD_ROOT"
    let buildDir = buildRoot <> "/build"
    return (cfg, buildDir)
  case eRes of
    Left e -> putFailure e
    Right (cfg, buildDir) -> do
      eMigrationRes <- runMigrator cfg $ do
        results <- runMigration
        _ <- liftIO $ removePathForcibly buildDir
        liftIO . print $ ("Writing Admin Details" :: String)
        _ <- liftIO $ writeAdmin buildDir (results^.migrationAdminAddress)
        artifacts <- forM (results^.migrationContractList) $ getContractAbis (buildDir <> "/contracts")
        liftIO $ forM_ (mconcat artifacts) writeArtifact
      case eMigrationRes of
        Left e -> putFailure e
        Right () -> putSuccess ("Build Artifacts written!" :: String)
