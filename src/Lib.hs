{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import           BlockApps.Bloc.API.Users
import           BlockApps.Bloc.API.Utils
import qualified BlockApps.Bloc.Client    as Bloc
import           BlockApps.Bloc.Crypto
import           BlockApps.Ethereum       (Address, Gas (..), Wei (..))
import           Control.Error
import           Control.Lens             (mapMOf, (&), (^.))
import           Control.Lens.TH          (makeLenses)
import           Control.Monad            (forM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger     (MonadLogger, logErrorN, logInfoN)
import           Data.Bifunctor           (first)
import           Data.Map.Strict          (Map)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Data.Yaml                (FromJSON, (.:), (.:?))
import qualified Data.Yaml                as Y
import           Numeric.Natural
import           Servant.Client
import           System.FilePath.Find     (always, contains, find)

--------------------------------------------------------------------------------

data MigrationError = ParseError Text
                    | BlocError Text
                    deriving (Show)

--------------------------------------------------------------------------------
-- | Manage Admin
--------------------------------------------------------------------------------

data AdminConfig =
  AdminConfig { _adminUsername :: UserName
              , _adminPassword :: Password
              , _adminFaucet   :: Text
              } deriving (Eq, Show)
makeLenses ''AdminConfig

createAdmin :: ClientEnv
            -> AdminConfig
            -> IO (Either MigrationError Address)
createAdmin clientEnv admin = do
  let postUsersUserRequest = PostUsersUserRequest (admin^.adminFaucet) (admin^.adminPassword)
  eAddr <- runClientM (Bloc.postUsersUser (admin^.adminUsername) postUsersUserRequest) clientEnv
  return $ first (BlocError . T.pack . show) eAddr

--------------------------------------------------------------------------------
-- | Parsing Yaml files for Contract info
--------------------------------------------------------------------------------

data Source = AsFilename
            | AsCode
            deriving (Eq, Show)

type family SourceType s where
  SourceType 'AsFilename = String
  SourceType 'AsCode = Text

data ContractForUpload s =
  ContractForUpload
    { _contractUploadName        :: Text
    , _contractUploadSource      :: SourceType s
    , _contractUploadInitialArgs :: Maybe (Map Text Text)
    , _contractUploadTxParams    :: Maybe TxParams
    , _contractUploadNonce       :: Maybe Natural
    }

makeLenses ''ContractForUpload

deriving instance Eq (SourceType s) => Eq (ContractForUpload s)
deriving instance Show (SourceType s) => Show (ContractForUpload s)

instance FromJSON (ContractForUpload 'AsFilename) where
  parseJSON (Y.Object o) =
    ContractForUpload <$> o .: "name"
                      <*> o .: "file"
                      <*> o .:? "args"
                      <*> o .:? "txParams"
                      <*> o .:? "value"

grabSourceCode :: FilePath -> String -> IO Text
grabSourceCode path filename = do
  matches <- find always (contains filename) path
  case matches of
    [fp] -> T.readFile (fp ++ "/" ++ filename)
    _    -> error $ "more than one match for: " ++ filename

withSourceCode :: ContractForUpload 'AsFilename -> IO (ContractForUpload 'AsCode)
withSourceCode = mapMOf contractUploadSource (grabSourceCode ".")

--------------------------------------------------------------------------------
-- | Deploy Contracts
--------------------------------------------------------------------------------

data Contract =
  Contract { _contractName    :: Text
           , _contractAddress :: Address
           } deriving (Eq, Show)
makeLenses ''Contract

deployContract :: (MonadIO m, MonadLogger m)
               => ClientEnv
               -> AdminConfig
               -> Address -- ^ the admin's address
               -> ContractForUpload 'AsFilename
               -> m (Either MigrationError Address)
deployContract env admin adminAddr contract = do
  contract' <- liftIO $ withSourceCode contract
  let req = PostUsersContractRequest
        { postuserscontractrequestSrc = contract'^.contractUploadSource
        , postuserscontractrequestPassword = admin^.adminPassword
        , postuserscontractrequestContract = contract'^.contractUploadName
        , postuserscontractrequestArgs = contract'^.contractUploadInitialArgs
        , postuserscontractrequestTxParams = contract'^.contractUploadTxParams
        , postuserscontractrequestValue = contract'^.contractUploadNonce
        }
  eAddr <- liftIO $ runClientM (Bloc.postUsersContract (admin^.adminUsername) adminAddr req) env
  case eAddr of
    Left e -> do
      let message = mconcat [ "Failed to deploy -- ["
                            , contract ^. contractUploadName
                            , "]-- " <> T.pack (show e)
                            ]
      _ <- logErrorN message
      return . Left . BlocError . T.pack . show $ e
    Right addr -> do
      _ <- logInfoN $ "Successfully deployed " <> contract ^. contractUploadName
      return . Right $ addr

deployContracts :: (MonadIO m, MonadLogger m)
                => ClientEnv
                -> AdminConfig
                -> Address -- ^ ownerAddress
                -> FilePath -- ^ location of contracts.yaml
                -> m (Either MigrationError [Contract])
deployContracts env admin ownerAddr contractYaml = do
  ecs <- liftIO $ Y.decodeFileEither contractYaml
  case ecs of
    Left e -> return . Left . ParseError . T.pack . show $ e
    Right cs -> runExceptT . forM cs $ \c -> do
      addr <- ExceptT $ deployContract env admin ownerAddr c
      return $ Contract (c^.contractUploadName) addr

--------------------------------------------------------------------------------
-- | Run the migration
--------------------------------------------------------------------------------

data MigrationResult =
  MigrationResult { _migrationAdminAddress  :: Address
                  , _migrationContractList :: [Contract]
                  } deriving (Eq, Show)
makeLenses ''MigrationResult

runMigration :: (MonadIO m, MonadLogger m)
             => ClientEnv
             -> AdminConfig
             -> FilePath -- ^ location of contracts.yaml
             -> m (Either MigrationError MigrationResult)
runMigration env admin contractYaml = do
  eUserAddress <- liftIO $ createAdmin env admin
  case eUserAddress of
      (Left e) -> (logErrorN $ T.pack (show e)) >> undefined
      (Right adminAddr) -> runExceptT $ do
        cs <- ExceptT $ deployContracts env admin adminAddr contractYaml
        return $ MigrationResult adminAddr cs
