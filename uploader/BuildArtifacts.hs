{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module BuildArtifacts where

import           BlocMigrations

import           BlockApps.Bloc21.API
import qualified BlockApps.Bloc21.Client as Bloc
import           BlockApps.Ethereum
import           BlockApps.Solidity.Xabi (MaybeNamed (..))
import           Control.Error
import           Control.Monad           (forM)
import           Control.Lens            ((^.))
import           Control.Lens.TH         (makeLenses)
import           Control.Monad.IO.Class
import           Data.Aeson              (encode, object, (.=))
import qualified Data.ByteString.Lazy    as BSL
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Data.Text              as T
import           Servant.Client          (ClientEnv(..), runClientM)
import           System.Directory        (createDirectoryIfMissing)

--------------------------------------------------------------------------------

maybeNamedToText :: MaybeNamed Address -> Text
maybeNamedToText (Named n) = n
maybeNamedToText (Unnamed a) = cs . addressString $ a

isNamedAndNotLatest :: MaybeNamed a -> Bool
isNamedAndNotLatest a = case a of
  Named a' -> if a' == "Latest" then False else True
  _ -> False

getContractAbis :: ClientEnv -> FilePath -> Contract -> ExceptT MigrationError IO [BuildArtifact]
getContractAbis env dir contract = do
  let (Contract cName _) = contract
  contracts <- fmapLT liftServantErr . ExceptT $ runClientM (Bloc.getContractsData cName) env
  let contracts' = filter isNamedAndNotLatest contracts
  liftIO . print $ "Getting ABIs for: " <> (T.intercalate ", " $ map maybeNamedToText contracts')
  forM contracts' $ \c -> do
    let ContractName cNameText = cName
        depNameText = maybeNamedToText c
    deets <- fmapLT (const $ throwFindError cNameText) . ExceptT $ runClientM (Bloc.getContractsContract cName c) env
    let bp = dir <> "/" <> cs  cNameText <> "/"
        fp = cs depNameText <> ".json"
    return $ BuildArtifact bp fp $ encode deets
  where
    throwFindError msg = FindContractError $ "Could not find Xabi for contract: " <> msg

data BuildArtifact =
  BuildArtifact { _buildPath :: FilePath
                , _buildFile :: FilePath
                , _buildArtifact :: BSL.ByteString
                }

makeLenses ''BuildArtifact

writeArtifact :: BuildArtifact -> IO ()
writeArtifact ba = do
 _ <- createDirectoryIfMissing True $ ba ^. buildPath
 BSL.writeFile (ba ^. buildPath <> ba^.buildFile) $ ba^.buildArtifact

writeAdmin :: FilePath -> Address -> IO ()
writeAdmin dir admin = do
  let adminFile = dir <> "/admin.json"
  _ <- createDirectoryIfMissing True dir
  BSL.writeFile adminFile . encode $ object ["adminAddress" .= admin]
