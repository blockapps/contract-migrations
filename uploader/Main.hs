{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BlockApps.Ethereum   (Address, stringAddress)
import           BlocMigrations
import           BuildArtifacts
import qualified Control.Error        as E
import           Control.Lens         ((^.))
import           Control.Monad.Except
import           Data.Monoid
import           System.Directory     (removePathForcibly)
import           System.Environment   (lookupEnv)

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
        madminAddr <- tryFindAddress
        results <- runMigration madminAddr
        _ <- liftIO $ removePathForcibly buildDir
        liftIO . print $ ("Writing Admin Details" :: String)
        _ <- liftIO $ writeAdmin buildDir (results^.migrationAdminAddress)
        artifacts <- forM (results^.migrationContractList) $ getContractAbis (buildDir <> "/contracts")
        liftIO $ forM_ (mconcat artifacts) writeArtifact
      case eMigrationRes of
        Left e   -> putFailure e
        Right () -> putSuccess ("Build Artifacts written!" :: String)

tryFindAddress :: ( MonadError MigrationError m
                  , MonadIO m
                  )
               => m (Maybe Address)
tryFindAddress = do
  maddrString <- liftIO $ lookupEnv "ADMIN_ADDRESS"
  case maddrString of
    Nothing -> return Nothing
    Just addrString -> case stringAddress addrString of
      Nothing -> throwError . EnvError $ "Could not parse ADMIN_ADDRESS as address"
      Just a -> do
        _ <- putSuccess $ "ADMIN_ADDRESS found: " <> addrString
        return . Just $ a
