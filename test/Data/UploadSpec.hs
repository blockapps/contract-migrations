{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.UploadSpec (spec) where

import           BlockApps.Bloc.API
import           BlockApps.Ethereum
import           Control.Concurrent
import           Control.Monad.Logger

import           Control.Error
import           Control.Lens
import           Data.ByteString (ByteString)
import qualified Data.Map.Strict                as Map
import           Data.Text
import qualified Data.Text.IO                   as T
import           Data.Yaml
import           Test.Hspec
import           Text.RawString.QQ

import           Network.HTTP.Client            (defaultManagerSettings,
                                                 newManager)
import           Network.Wai.Handler.Warp       (run, testWithApplication)
import           Servant
import           Servant.Client
import           Servant.Mock
import           System.IO.Unsafe               (unsafePerformIO)

import           Lib


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
        cAddr <- runExceptT (deployContract bloc adminConfig addr =<< exampleUpload')
        cAddr `shouldSatisfy` isRight
      it "can upload a many contracts" $ \port -> do
        bloc <- mockBlocClient port
        Right addr <- runExceptT $ createAdmin bloc adminConfig
        newCs <- (runExceptT $ deployContracts bloc adminConfig addr "./contracts/contracts.yaml")
        print $ newCs
        let (Right newCs') = newCs
        (newCs' !! 0) ^. contractName  `shouldBe` "IdentityAccessManager"

--------------------------------------------------------------------------------
-- utils
--------------------------------------------------------------------------------

adminConfig :: AdminConfig
adminConfig = AdminConfig "Admin" "Password" "666"

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
  { _contractUploadName = "Owned"
  , _contractUploadSource = "Owned.sol"
  , _contractUploadInitialArgs = Just $ Map.fromList [("name", "Bob") , ("age", "23")]
  , _contractUploadTxParams = Just (TxParams (Just $ Gas 1) (Just $ Wei 2) (Just $ Nonce 3))
  , _contractUploadNonce = Just 10
  }

exampleUpload' :: ExceptT MigrationError IO (ContractForUpload 'AsCode)
exampleUpload' = withSourceCode exampleUpload
