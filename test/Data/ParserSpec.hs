{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.ParserSpec (spec) where

import           Control.Error
import           BlockApps.Bloc.API
import           BlockApps.Ethereum
import           Control.Lens
import           Data.ByteString
import           Data.Either
import qualified Data.Map.Strict    as Map
import           Data.Text
import qualified Data.Text.IO       as T
import           Data.Yaml
import           Test.Hspec
import           Text.RawString.QQ

import           Lib

spec :: Spec
spec = yamlSpec

yamlSpec :: Spec
yamlSpec = do
  describe "it can parse a contracts.yaml file" $ do
    it "can get a list of ContractForUpload" $ do
      let Right cs :: Either ParseException [ContractForUpload 'AsFilename] = decodeEither' contractYaml
      cs `shouldBe` exampleList
    it "can read and replace sourcode" $ do
      let Right cs :: Either ParseException [ContractForUpload 'AsFilename] = decodeEither' contractYaml
      raw1 <- T.readFile "contracts/Modifiers/Owned.sol"
      Right c1 <- runExceptT . withSourceCode $ cs !! 0
      raw1 `shouldBe` c1 ^. contractUploadSource
      raw2 <- T.readFile "contracts/IdentityAccessManager.sol"
      Right c2 <- runExceptT . withSourceCode $ cs !! 1
      raw2 `shouldBe` c2 ^. contractUploadSource

--------------------------------------------------------------------------------
-- | Constants
--------------------------------------------------------------------------------
contractYaml :: ByteString
contractYaml = [r|
- name: Owned
  file: Owned.sol
  args:
    name: Bob
    age: '23'
  txParams:
    gasLimit: 1
    gasPrice: 2
    nonce:  3
  value: 10

- name: IdentityAccessManager
  file: IdentityAccessManager.sol
|]

exampleList :: [ContractForUpload 'AsFilename]
exampleList = [ex1, ex2]
  where
    ex1, ex2 :: ContractForUpload 'AsFilename
    ex1 = ContractForUpload
      { _contractUploadName = "Owned"
      , _contractUploadSource = "Owned.sol"
      , _contractUploadInitialArgs = Just $ Map.fromList [("name", "Bob") , ("age", "23")]
      , _contractUploadTxParams = Just (TxParams (Just $ Gas 1) (Just $ Wei 2) (Just $ Nonce 3))
      , _contractUploadNonce = Just 10
      }
    ex2 = ContractForUpload "IdentityAccessManager" "IdentityAccessManager.sol" Nothing Nothing Nothing
