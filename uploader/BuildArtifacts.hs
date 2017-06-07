{-# LANGUAGE OverloadedStrings #-}

module BuildArtifacts where

import           BlocMigrations

import           BlockApps.Bloc21.API
import qualified BlockApps.Bloc21.Client as Bloc
import           BlockApps.Solidity.Xabi (MaybeNamed (..))
import           Control.Error
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (encode)
import qualified Data.ByteString.Lazy    as BSL
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import           Servant.Client          (ClientEnv(..), runClientM)
import           System.Directory        (createDirectoryIfMissing)

getContractAbi :: ClientEnv -> FilePath -> Contract -> ExceptT MigrationError IO (FilePath, BSL.ByteString)
getContractAbi env dir contract = do
  let (Contract cName cAddr) = contract
      ContractName cNameText = cName
  edeets <- liftIO $ runClientM (Bloc.getContractsContract cName (Unnamed cAddr)) env
  case edeets of
    Left e -> do
      liftIO $ print e
      throwE . FindContractError $ "Could not find Xabi for contract: " <> cNameText
    Right deets -> return $ (dir <> "/" <> cs  cNameText <> ".json", encode deets)

makeBuildDir :: FilePath -> IO ()
makeBuildDir buildDir = createDirectoryIfMissing True buildDir
