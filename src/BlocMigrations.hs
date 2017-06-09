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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module BlocMigrations where

import           BlockApps.Bloc21.API
import qualified BlockApps.Bloc21.Client      as Bloc
import           BlockApps.Ethereum           (Address, addressString)
import           BlockApps.Solidity.ArgValue
import           BlockApps.Strato.Types       (Strung (..))
import qualified Control.Error                as E
import           Control.Lens                 (use, (%=), (&), (.=), (.~), (^.),
                                               _1, _2, view)
import           Control.Lens.TH              (makeLenses)
import           Control.Monad                (forM)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.Graph                   as G
import           Data.List                    (dropWhile, last, reverse,
                                               takeWhile)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Monoid                  ((<>))
import qualified Data.Set                     as S
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.IO                 as T
import           Data.Yaml                    (FromJSON, (.:), (.:?))
import qualified Data.Yaml                    as Y
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Numeric.Natural
import           Servant.Client
import           System.Environment           (lookupEnv)
import           System.FilePath.Find         (always, contains, find)

import           Text.ParserCombinators.ReadP
import qualified Text.PrettyPrint.ANSI.Leijen as PP

--------------------------------------------------------------------------------
-- | Migrator
--------------------------------------------------------------------------------

data MigrationError = ParseError Text
                    | BlocError Text
                    | EnvError Text
                    | FindContractError Text
                    deriving (Show)

liftServantErr :: ServantError -> MigrationError
liftServantErr e = case e of
  FailureResponse _ _ bdy -> BlocError $ cs bdy
  _ -> BlocError "Unknown Bloc Error, check bloc logs."

data AdminConfig =
  AdminConfig { _adminUsername :: UserName
              , _adminPassword :: Password
              , _adminFaucet   :: Bool
              } deriving (Eq, Show)
makeLenses ''AdminConfig

data VerboseMode = DEBUG
                 | SILENT
                  deriving (Show, Read, Eq)

data ContractFileConfig =
  ContractFileConfig { _contractsYaml :: FilePath
                     , _contractsDir :: FilePath
                     }
makeLenses ''ContractFileConfig

data MigrationConfig =
  MigrationConfig { _adminConfig :: AdminConfig
                  , _blocClient :: ClientEnv
                  , _verboseMode :: VerboseMode
                  , _contractFileConfig :: ContractFileConfig
                  }

makeLenses ''MigrationConfig

newtype Migrator a = Migrator { runMigrator' :: ExceptT MigrationError (ReaderT MigrationConfig IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError MigrationError, MonadReader MigrationConfig)

runMigrator :: MigrationConfig -> forall a . Migrator a -> IO (Either MigrationError a)
runMigrator cfg = flip runReaderT cfg . runExceptT . runMigrator'

(!?) :: MonadError e m => m (Maybe a) -> e -> m a
(!?) m e = do
  ma <- m
  case ma of
    Nothing -> throwError e
    Just a -> return a

--------------------------------------------------------------------------------
-- | Formatting
--------------------------------------------------------------------------------

putSuccess :: (Show a, MonadIO m) => a -> m ()
putSuccess = liftIO . print . PP.green . PP.pretty . show

putFailure :: (Show a, MonadIO m) => a -> m ()
putFailure = liftIO . print . PP.red . PP.pretty . show

--------------------------------------------------------------------------------
-- | Environment
--------------------------------------------------------------------------------

getAdminFromEnv :: ( MonadError MigrationError m
                   , MonadIO m
                   )
                => m AdminConfig
getAdminFromEnv = do
  usrName <- UserName . T.pack <$>
    ((liftIO $ lookupEnv "BLOC_ADMIN_USERNAME") !? EnvError "Could Not Find BLOC_ADMIN_USERNAME.")
  pwd <- Password . T.encodeUtf8 . T.pack <$>
    (liftIO (lookupEnv "BLOC_ADMIN_PASSWORD") !? EnvError "Could Not Find BLOC_ADMIN_PASSWORD.")
  faucetOn <- read <$> ((liftIO $ lookupEnv "BLOC_ADMIN_FAUCET") !? EnvError "Could not find BLOC_ADMIN_FAUCET.")
  return $ AdminConfig usrName pwd faucetOn

makeBlocEnv :: ( MonadError MigrationError m
               , MonadIO m
               )
            => m ClientEnv
makeBlocEnv = do
    scheme <- ((liftIO $ lookupEnv "BLOC_SCHEME") !? EnvError "Couldn't find BLOC_SCHEME") >>= mkScheme
    host <- (liftIO $ lookupEnv "BLOC_HOST") !? EnvError "Couldn't find BLOC_HOST"
    port <- read <$> ((liftIO $ lookupEnv "BLOC_PORT") !? EnvError "Couldn't find BLOC_PORT")
    path <- (liftIO $ lookupEnv "BLOC_PATH") !? EnvError "Couldn't find BLOC_PATH"
    mgr <- liftIO $ newManager defaultManagerSettings
    return $ ClientEnv mgr (BaseUrl scheme host port path)
  where
    mkScheme :: MonadError MigrationError m => String -> m Scheme
    mkScheme "Http"  = return Http
    mkScheme "Https" = return Https
    mkScheme _       = throwError . EnvError $ "Scheme must be either Http or Https."

makeFileConfig :: ( MonadError MigrationError m
                  , MonadIO m
                  )
               => m ContractFileConfig
makeFileConfig = do
  yml <- (liftIO $ lookupEnv "CONTRACTS_YAML") !? EnvError "Couldn't find CONTRACTS_YAML"
  cDir <- (liftIO $ lookupEnv "CONTRACTS_DIR") !? EnvError "Couldn't find CONTRACTS_DIR"
  return $ ContractFileConfig yml cDir

mkMigrationConfig :: ( MonadError MigrationError m
                     , MonadIO m
                     )
                  => m MigrationConfig
mkMigrationConfig = do
  admin <- getAdminFromEnv
  bloc <- makeBlocEnv
  verbose <- liftIO $ maybe SILENT read <$> lookupEnv "VERBOSE_CONTRACT_UPLOAD"
  fc <- makeFileConfig
  return $ MigrationConfig admin bloc verbose fc

createAdmin :: ( MonadError MigrationError m
               , MonadIO m
               , MonadReader MigrationConfig m
               )
            => m Address
createAdmin = do
    admin <- view adminConfig
    env <- view blocClient
    let postUsersUserRequest = (admin^.adminPassword)
    eresp <- liftIO $ runClientM (Bloc.postUsersUser (admin^.adminUsername)  (admin^.adminFaucet) postUsersUserRequest) env
    case eresp of
      Left e -> throwError . liftServantErr $ e
      Right addr -> return addr

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
    , _contractUploadInitialArgs :: Maybe (Map Text ArgValue)
    , _contractUploadTxParams    :: Maybe TxParams
    , _contractUploadNonce       :: Maybe (Strung Natural)
    , _contractUploadIndexed     :: Maybe [Text]
    }

makeLenses ''ContractForUpload

deriving instance Eq (SourceType s) => Eq (ContractForUpload s)
deriving instance Show (SourceType s) => Show (ContractForUpload s)

instance FromJSON (ContractForUpload 'AsFilename) where
  parseJSON (Y.Object o) =
    ContractForUpload <$> o .: "name"
                      <*> o .:  "file"
                      <*> o .:? "args"
                      <*> o .:? "txParams"
                      <*> o .:? "value"
                      <*> o .:? "index"
  parseJSON v = fail $ "Needed an object, found: " ++ show v

-- | This looks for any directory that contains a file with a
-- name that contains 'filename' and returns the found file,
-- but errors out if there are none or more thatn one.
findUniqueFile :: ( MonadError MigrationError m
                  , MonadIO m
                  )
               => FilePath
               -> String
               -> m FilePath
findUniqueFile path filename = do
  matches <- liftIO $ find always (contains filename) path
  case matches of
    [] -> throwError . FindContractError $
            "No Contracts matching filname: " <> T.pack filename <> " in " <> T.pack path
    [fp] -> return $ fp <> "/" <> filename
    _    -> throwError . FindContractError $
              "more than one match for: " <> T.pack filename <> " in " <> T.pack path

-- | searches for a file and grabs the source code.
grabSourceCode :: ( MonadError MigrationError m
                  , MonadIO m
                  )
               => FilePath
               -> String
               -> m Text
grabSourceCode path filename = findUniqueFile path filename >>= liftIO . T.readFile

isImportStatement :: Text -> Bool
isImportStatement = T.isPrefixOf "import"

importsParser :: ReadP String
importsParser = fmap last $ do
   _ <- void $ string "import"
   _ <- skipSpaces
   between quoter quoter $
     sepBy1 (many1 (satisfy (/= ';'))) (char '/')
  where
    quoter =  satisfy (`elem` ("'\"" :: String))

-- | get all the dependencies for a solidity file.
grabImports :: ( MonadError MigrationError m
               , MonadIO m
               )
            => Text
            -> m [FilePath]
grabImports sourceCode =
    let ls = T.lines sourceCode
        importStrings = map T.unpack $ takeWhile isImportStatement ls
    in forM importStrings $ \importString ->
         case readP_to_S importsParser importString of
           ((imp,_) : _) -> return imp
           other -> do
             putFailure other
             throwError . ParseError $ "Could not parse import statement: " <>
                   T.pack importString

type DependencyGraph = (G.Graph, G.Vertex -> (FilePath, FilePath, [FilePath]), FilePath -> Maybe G.Vertex)

data GraphState =
  GraphState { _edgeSet         :: Map FilePath (S.Set FilePath)
             , _visitedVertices :: S.Set FilePath
             }
makeLenses ''GraphState

-- | This get's the dependency graph for the files. Again, it rests on the assumption
-- that in any contracts project, there every contract filename is unique.
grabDependencyGraph :: ( MonadError MigrationError m
                       , MonadIO m
                       , MonadReader MigrationConfig m
                       )
                    => FilePath
                    -> [FilePath]
                    -> m DependencyGraph
grabDependencyGraph _main fps = do
    let initState = GraphState (Map.singleton _main (S.fromList fps)) (S.singleton _main)
    GraphState edges _ <- execStateT (grabDependencyGraph' fps) initState
    return . G.graphFromEdges . map (\(k,ns) -> (k, k, S.toList ns)) $ Map.toList edges
  where
    grabDependencyGraph' filepaths = do
      nbrs <- lift $ traverse grabNeighbors filepaths
      edgeSet %= \known -> foldr addEdges known nbrs
      visited <- use visitedVertices
      let visited' = foldr (S.insert . fst) visited nbrs
          nextVertices = S.toList $ S.unions (map snd nbrs) `S.difference` visited'
      case nextVertices of
        [] -> return ()
        _ -> do
          visitedVertices .= visited'
          grabDependencyGraph' nextVertices
    grabNeighbors filepath = do
      contractDir <- view $ contractFileConfig . contractsDir
      nbrs <- grabSourceCode contractDir filepath >>= grabImports
      return (filepath, S.fromList nbrs)
    addEdges :: (FilePath, S.Set FilePath) -> Map FilePath (S.Set FilePath) -> Map FilePath (S.Set FilePath)
    addEdges (v, ns) m = case Map.lookup v m of
      Nothing  -> Map.insert v ns m
      Just ns' -> Map.insert v (ns `S.union` ns') m

-- | Collect all the source code for a list of filenames.
readAndTrimFiles :: ( MonadError MigrationError m
                    , MonadIO m
                    , MonadReader MigrationConfig m
                    )
                 => [FilePath]
                 -> m Text
readAndTrimFiles fps = do
    contractDir <- view $ contractFileConfig . contractsDir
    sources <- traverse (findUniqueFile contractDir >=>  fmap trimDependencies . liftIO . T.readFile) fps
    return . T.unlines . map T.strip $ sources

trimDependencies :: Text -> Text
trimDependencies = T.strip . T.unlines . dropWhile isImportStatement . T.lines

-- | replace the contract source file with the source code in Text form.
withSourceCode :: ( MonadError MigrationError m
                  , MonadIO m
                  , MonadReader MigrationConfig m
                  )
               => ContractForUpload 'AsFilename
               -> m (ContractForUpload 'AsCode)
withSourceCode c = do
  contractDir <- view $ contractFileConfig . contractsDir
  sourceCode <- grabSourceCode contractDir $ c^.contractUploadSource
  deps <- grabImports sourceCode
  case deps of
    [] -> return $ c & contractUploadSource .~ sourceCode
    ds -> do
      let _main = c^.contractUploadSource
      g <- grabDependencyGraph _main ds
      let orderedFileIndices = G.topSort (g^._1)
          vertexMapping = g^._2
          orderedFiles = reverse $ map (\v -> vertexMapping v ^._1) orderedFileIndices
      fullSource <- readAndTrimFiles orderedFiles
      return $ c & contractUploadSource .~ fullSource

--------------------------------------------------------------------------------
-- | Deploy Contracts
--------------------------------------------------------------------------------

data Contract =
  Contract { _contractName    :: ContractName
           , _contractAddress :: Address
           } deriving (Eq, Show)
makeLenses ''Contract

-- |  deploys a contract.
deployContract :: ( MonadError MigrationError m
                  , MonadIO m
                  , MonadReader MigrationConfig m
                  )
               => Address -- ^ the admin's address
               -> ContractForUpload 'AsCode
               -> m Address
deployContract adminAddr contract = do
  admin <- view adminConfig
  env <- view blocClient
  verbose <- view verboseMode
  let postContractReq = PostUsersContractRequest
        { postuserscontractrequestSrc = contract^.contractUploadSource
        , postuserscontractrequestPassword = admin^.adminPassword
        , postuserscontractrequestContract = Just $ contract^.contractUploadName
        , postuserscontractrequestArgs = contract^.contractUploadInitialArgs
        , postuserscontractrequestTxParams = contract^.contractUploadTxParams
        , postuserscontractrequestValue = contract^.contractUploadNonce
        }
      indexContractReq = PostCompileRequest
        { postcompilerequestSearchable = contract^.contractUploadIndexed
        , postcompilerequestContractName = Just $ contract^.contractUploadName
        , postcompilerequestSource = contract^.contractUploadSource}
  when (E.isJust $ postcompilerequestSearchable indexContractReq) $ do
    let searchableList = E.fromMaybe [] $ postcompilerequestSearchable indexContractReq
    _ <- liftIO $ runClientM (Bloc.postContractsCompile [indexContractReq]) env
    putSuccess $ "Indexed Contracts -- " <> T.intercalate ", " searchableList
  eresp <- liftIO $ runClientM (Bloc.postUsersContract (admin^.adminUsername) adminAddr postContractReq) env
  case eresp of
    Left serr -> throwError $ message serr
    Right success -> do
      putSuccess $ "Deployed Contract: " <> contract^.contractUploadName
      when (verbose == DEBUG) $
        liftIO . putStrLn . T.unpack $ contract^.contractUploadSource
      return success
  where
    message :: ServantError -> MigrationError
    message e =
      let BlocError eTxt = liftServantErr $ e
      in BlocError $ mconcat [ "Failed to deploy -- ["
                                   , contract ^. contractUploadName
                                   , "]-- "
                                   , eTxt
                                   ]

-- | deploy all the contracts listed in the contracts.yaml file.
deployContracts :: ( MonadError MigrationError m
                   , MonadIO m
                   , MonadReader MigrationConfig m
                   )
                => Address -- ^ ownerAddress
                -> m [Contract]
deployContracts ownerAddr = do
  yml <- view $ contractFileConfig . contractsYaml
  ecs <- liftIO $ Y.decodeFileEither yml
  case ecs of
    Left e -> throwError . ParseError . T.pack . show $ e
    Right contracts -> forM contracts $ \c -> do
      c' <- withSourceCode c
      addr <- deployContract ownerAddr c'
      return $ Contract (ContractName $ c^.contractUploadName) addr

--------------------------------------------------------------------------------
-- | Run the migration
--------------------------------------------------------------------------------

data MigrationResult =
  MigrationResult { _migrationAdminAddress :: Address
                  , _migrationContractList :: [Contract]
                  } deriving (Eq, Show)
makeLenses ''MigrationResult

-- | This will create the admin user and deploy all the contracts,
-- returning the summary in the MigrationResult
runMigration :: ( MonadError MigrationError m
                , MonadIO m
                , MonadReader MigrationConfig m
                )
             => m MigrationResult
runMigration = do
  verbose <- view verboseMode
  liftIO . print $ "Verbose Level: " <> show verbose
  adminAddress <- createAdmin
  putSuccess $ "Admin created! Admin Address: " <> addressString adminAddress
  liftIO . print $ ("Deploying Contracts" :: String)
  contracts <- deployContracts adminAddress
  putSuccess ("Successfully Depployed Contracts" :: String)
  return $ MigrationResult adminAddress contracts
