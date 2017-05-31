{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.UploadSpec (spec) where

import           BlockApps.Strato.Types
import           BlockApps.Bloc21.API        hiding (mockBloc)
import           BlockApps.Ethereum
import           Control.Concurrent
import           Control.Monad.Logger

import           Control.Error
import           Control.Lens
import           Data.ByteString             (ByteString)
import qualified Data.Map.Strict             as Map
import           Data.Text
import qualified Data.Text.IO                as T
import           Data.Yaml
import           Test.Hspec
import           Text.RawString.QQ

import           Network.HTTP.Client         (defaultManagerSettings,
                                              newManager)
import           Network.Wai.Handler.Warp    (run, testWithApplication)
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
adminSpec = do
  describe "can make an admin user" $ do
    around (testWithApplication . return $ mockBloc) $ do
      it "can make an admin user" $ \port -> do
        bloc <- mockBlocClient port
        eAddr <- runExceptT $ createAdmin bloc adminConfig
        eAddr `shouldSatisfy` isRight

uploadSpec :: Spec
uploadSpec = do
  describe "it can upload contracts" $ do
    around (testWithApplication . return $ mockBloc) $ do
      it "can upload a single contract" $ \port -> do
        bloc <- mockBlocClient port
        Right addr <- runExceptT $ createAdmin bloc adminConfig
        cAddr <- runExceptT (exampleUpload'>>= \c -> deployContract bloc adminConfig addr c SILENT)
        cAddr `shouldSatisfy` isRight
      it "can upload a many contracts" $ \port -> do
        bloc <- mockBlocClient port
        Right addr <- runExceptT $ createAdmin bloc adminConfig
        newCs <- (runExceptT $ deployContracts bloc adminConfig addr "./contracts/contracts.yaml" "./contracts" SILENT)
        let (Right newCs') = newCs
        print newCs
        (newCs' !! 0) ^. contractName  `shouldBe` Just "IdentityAccessManager"

--------------------------------------------------------------------------------
-- utils
--------------------------------------------------------------------------------

adminConfig :: AdminConfig
adminConfig = AdminConfig "Admin" "Password" True

mockBlocClient :: Int -> IO ClientEnv
mockBlocClient port = do
  mgr <- newManager defaultManagerSettings
  let url = BaseUrl Http "localhost" port ""
  return $ ClientEnv mgr url

mockBloc :: Application
mockBloc = serve blocApi mockBloc
  where
    mockBloc :: Server BlocAPI
    mockBloc = mock blocApi Proxy

exampleUpload :: ContractForUpload 'AsFilename
exampleUpload = ContractForUpload
  { _contractUploadName = Just "Owned"
  , _contractUploadSource = "Owned.sol"
  , _contractUploadInitialArgs = Just $ Map.fromList [("name", ArgString "Bob") , ("age", ArgInt 23)]
  , _contractUploadTxParams = Just (TxParams (Just $ Gas 1) (Just $ Wei 2) (Just $ Nonce 3))
  , _contractUploadNonce = Just (Strung 10)
  , _contractUploadIndexed = Just []
  }

exampleUpload' :: ExceptT MigrationError IO (ContractForUpload 'AsCode)
exampleUpload' = withSourceCode "./contracts" exampleUpload
