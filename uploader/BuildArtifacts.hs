{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module BuildArtifacts where

import           BlocMigrations

import           BlockApps.Bloc21.API
import qualified BlockApps.Bloc21.Client as Bloc
import           BlockApps.Ethereum
import           BlockApps.Solidity.Xabi (MaybeNamed (..), ContractDetails(..))
import           Control.Lens            (view, (^.))
import           Control.Lens.TH         (makeLenses)
import           Control.Monad           (forM)
import           Control.Monad.Except
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader
import           Data.Aeson              (encode, object, (.=))
import qualified Data.ByteString.Lazy    as BSL
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Servant.Client          (runClientM)
import           System.Directory        (createDirectoryIfMissing)

--------------------------------------------------------------------------------

maybeNamedToText :: MaybeNamed Address -> Text
maybeNamedToText (Named n)   = n
maybeNamedToText (Unnamed a) = cs . addressString $ a

isNamedAndNotLatest :: MaybeNamed a -> Bool
isNamedAndNotLatest a = case a of
  Named a' -> if a' == "Latest" then False else True
  _        -> False

getContractAbis :: ( MonadError MigrationError m
                   , MonadIO m
                   , MonadReader MigrationConfig m
                   )
                 => FilePath
                -> Contract
                -> m [BuildArtifact]
getContractAbis dir contract = do
  env <- view blocClient
  let (Contract cName _) = contract
  contracts <- do
    ecs <- liftIO $ runClientM (Bloc.getContractsData cName) env
    case ecs of
      Left e     -> throwError . liftServantErr $ e
      Right cnts -> return cnts
  let contracts' = filter isNamedAndNotLatest contracts
  liftIO . print $ "Getting ABIs for: " <> (T.intercalate ", " $ map maybeNamedToText contracts')
  forM contracts' $ \c -> do
    let ContractName cNameText = cName
        depNameText = maybeNamedToText c
    deets <- do
      edeets <- liftIO $ runClientM (Bloc.getContractsContract cName c) env
      case edeets of
        Left e   -> throwError . liftServantErr $ e
        Right ds -> return ds
    let bp = dir <> "/" <> cs  cNameText <> "/"
        fp = cs depNameText <> ".json"
    return $ BuildArtifact bp fp $ encode
      deets {contractdetailsAddress = Just .Named . T.pack . addressString $ contract ^. contractAddress}

data BuildArtifact =
  BuildArtifact { _buildPath     :: FilePath
                , _buildFile     :: FilePath
                , _buildArtifact :: BSL.ByteString
                }

makeLenses ''BuildArtifact

writeArtifact :: MonadIO m => BuildArtifact -> m ()
writeArtifact ba = liftIO $ do
 _ <- createDirectoryIfMissing True $ ba ^. buildPath
 BSL.writeFile (ba ^. buildPath <> ba^.buildFile) $ ba^.buildArtifact

writeAdmin :: MonadIO m => FilePath -> Address -> m ()
writeAdmin dir admin = liftIO $ do
  let adminFile = dir <> "/admin.json"
  _ <- createDirectoryIfMissing True dir
  BSL.writeFile adminFile . encode $ object ["adminAddress" .= admin]
