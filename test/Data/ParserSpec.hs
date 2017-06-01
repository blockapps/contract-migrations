{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.ParserSpec (spec) where

import           BlockApps.Strato.Types
import           BlockApps.Bloc21.API
import           BlockApps.Ethereum
import           BlockApps.Solidity.ArgValue
import           BlocMigrations
import           Control.Error
import           Control.Lens
import           Data.ByteString              (ByteString)
import qualified Data.Graph                   as G
import qualified Data.List                    as L
import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Data.Yaml
import           Test.Hspec
import           Text.ParserCombinators.ReadP
import           Text.RawString.QQ

spec :: Spec
spec = do
  importParserSpec
  yamlSpec
  fileParserSpec


importParserSpec :: Spec
importParserSpec = do
  describe "can parse import statements" $ do
    it "can parse a single import statement" $ do
      let res = readP_to_S importsParser "import \"../Thing/Contract.sol\";"
      (fst . head $ res) `shouldBe` "Contract.sol"
    it "can parse multiple import statements" $ do
      imps <- runExceptT $ grabImports importStatements
      let Right imps' = imps
      length imps' `shouldBe` 5

yamlSpec :: Spec
yamlSpec = do
  describe "it can parse a contracts.yaml file" $ do
    it "can get a list of ContractForUpload" $ do
      let Right cs :: Either ParseException [ContractForUpload 'AsFilename] = decodeEither' contractYaml
      cs `shouldBe` exampleList
--    it "can read and replace sourcode" $ do
--      let Right cs :: Either ParseException [ContractForUpload 'AsFilename] = decodeEither' contractYaml
--      raw1 <- T.readFile "contracts/Modifiers/Owned.sol"
--      Right c1 <- runExceptT . withSourceCode $ cs !! 0
--      raw1 `shouldBe` c1 ^. contractUploadSource
--      raw2 <- T.readFile "contracts/IdentityAccessManager.sol"
--      Right c2 <- runExceptT . withSourceCode $ cs !! 1
--      raw2 `shouldBe` c2 ^. contractUploadSource

fileParserSpec :: Spec
fileParserSpec = do
  describe "it can gather and files into blob based on imports" $ do
    it "can find import statements" $ do
      Right code <- runExceptT $ grabSourceCode "./contracts" "Simple.Sol"
      Right len <- runExceptT $ fmap L.length $  grabImports code
      len `shouldBe` 4
    it "can grab a dependency set with the right size" $ do
      eimps <- runExceptT $ grabSourceCode "./contracts" "Simple.Sol" >>= grabImports
      eimps `shouldSatisfy` isRight
      let Right imps = eimps
      eGdata <- runExceptT $ grabDependencyGraph "Simple.Sol" imps "./contracts"
      ((^. _1) <$> eGdata) `shouldSatisfy` isRight
      let Right (g, _, _) = eGdata
      (L.length . G.vertices $ g) `shouldBe` 8
    it "can properly trim off imports of one file" $ do
      Right t <- runExceptT . fmap T.strip $ readAndTrimFiles ["Simple.Sol"] "."
      t' <- fmap T.strip $ T.readFile "./contracts/SimpleTrimmed.sol"
      T.strip t `shouldBe` T.strip t'
    it "can properly read, trim, and concat two files" $ do
      Right output <- runExceptT . fmap T.strip $ readAndTrimFiles
        ["Simple.Sol", "IdentityAccessManager.sol"] "."
      t <- T.readFile "./contracts/Simple.sol"
      t' <- T.readFile "./contracts/IdentityAccessManager.sol"
      T.strip output `shouldBe` (T.strip . T.unlines . map (T.strip . trimDependencies) $ [t,t'])


--------------------------------------------------------------------------------
-- | Constants
--------------------------------------------------------------------------------
importStatements :: T.Text
importStatements = T.unlines $
  [ "import \"./Permissions/Owned.sol\";"
  , "import \"./Permissions/ReadPermissioned.sol\";"
  , "import \"./Storage/BasicUserStorage.sol\";"
  , "import \"./Storage/StorageBlob.sol\";"
  , "import \"./Login.sol\";"
  ]

contractYaml :: ByteString
contractYaml = [r|
- name: Owned
  file: Owned.sol
  args:
    name: "Bob"
    age: 23
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
      , _contractUploadInitialArgs = Just $ Map.fromList [("name", ArgString "Bob") , ("age", ArgInt 23)]
      , _contractUploadTxParams = Just (TxParams (Just $ Gas 1) (Just $ Wei 2) (Just $ Nonce 3))
      , _contractUploadNonce = Just (Strung 10)
      , _contractUploadIndexed = Nothing
      }
    ex2 = ContractForUpload "IdentityAccessManager" "IdentityAccessManager.sol" Nothing Nothing Nothing Nothing
