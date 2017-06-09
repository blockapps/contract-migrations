{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.UploadSpec (spec) where

import           BlockApps.Bloc21.API        hiding (mockBloc)
import           BlockApps.Ethereum
import           BlockApps.Strato.Types
import           Control.Error
import           Control.Lens
import qualified Data.Map.Strict             as Map
import           Test.Hspec

import           Network.HTTP.Client         (defaultManagerSettings,
                                              newManager)
import           Network.Wai.Handler.Warp    (testWithApplication)
import           Servant
import           Servant.Client
import           Servant.Mock
import           System.IO.Unsafe            (unsafePerformIO)

import           BlockApps.Solidity.ArgValue
import           BlocMigrations

spec :: Spec
spec = do
  adminSpec
  uploadSpec

adminSpec :: Spec
adminSpec =

  describe "can make an admin user" $

    around (testWithApplication . return $ mockBloc) $

      it "can make an admin user" $ \port -> do
        let cfg = mkConfig port
        eAddr <- runMigrator cfg $ createAdmin
        eAddr `shouldSatisfy` isRight

uploadSpec :: Spec
uploadSpec =

  describe "it can upload contracts" $
    around (testWithApplication . return $ mockBloc) $ do

      it "can upload a single contract" $ \port -> do
        let cfg = mkConfig port
        Right addr <- runMigrator cfg $ createAdmin
        cAddr <- runMigrator cfg $ (exampleUpload' >>= \c -> deployContract addr c)
        cAddr `shouldSatisfy` isRight

      it "can upload a many contracts" $ \port -> do
        let cfg = mkConfig port
        Right addr <- runMigrator cfg $ createAdmin
        newCs <- runMigrator cfg $ deployContracts addr
        let (Right newCs') = newCs
        Prelude.head newCs' ^. contractName  `shouldBe` "IdentityAccessManager"

--------------------------------------------------------------------------------
-- utils
--------------------------------------------------------------------------------

admnConfig :: AdminConfig
admnConfig = AdminConfig "Admin" "Password" True

mockBlocClient :: Int -> IO ClientEnv
mockBlocClient port = do
  mgr <- newManager defaultManagerSettings
  let url = BaseUrl Http "localhost" port ""
  return $ ClientEnv mgr url

mockBloc :: Application
mockBloc = serve blocApi mockBlocServer
  where
    mockBlocServer :: Server BlocAPI
    mockBlocServer = mock blocApi Proxy

mkConfig :: Int -> MigrationConfig
mkConfig port = unsafePerformIO $ do
  let cf = ContractFileConfig "./contracts/contracts.yaml" "./contracts"
  bloc <- mockBlocClient port
  return $ MigrationConfig admnConfig bloc SILENT cf
{-# NOINLINE mkConfig #-}

exampleUpload :: ContractForUpload 'AsFilename
exampleUpload = ContractForUpload
  { _contractUploadName = "Owned"
  , _contractUploadSource = "Owned.sol"
  , _contractUploadInitialArgs = Just $ Map.fromList [("name", ArgString "Bob") , ("age", ArgInt 23)]
  , _contractUploadTxParams = Just (TxParams (Just $ Gas 1) (Just $ Wei 2) (Just $ Nonce 3))
  , _contractUploadNonce = Just (Strung 10)
  , _contractUploadIndexed = Just []
  }

exampleUpload' :: Migrator (ContractForUpload 'AsCode)
exampleUpload' = withSourceCode exampleUpload
