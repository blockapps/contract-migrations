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

module BlocMigrations where

import           BlockApps.Bloc.API.Users
import           BlockApps.Bloc.API.Utils
import qualified BlockApps.Bloc.Client    as Bloc
import           BlockApps.Bloc.Crypto
import           BlockApps.Ethereum       (Address)
import           Control.Error
import           Control.Lens             (use, (%=), (&), (.=), (.~), (^.), _1, _2)
import           Control.Lens.TH          (makeLenses)
import           Control.Monad            (forM)
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.Bifunctor           (first)
import qualified Data.Graph               as G
import           Data.List                (dropWhile, last, reverse, takeWhile)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Monoid              ((<>))
import qualified Data.Set                 as S
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
                    | FindContractError Text
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
            -> ExceptT MigrationError IO Address
createAdmin clientEnv admin =
    let postUsersUserRequest = PostUsersUserRequest (admin^.adminFaucet) (admin^.adminPassword)
    in ExceptT $ fmap (first message) $
         runClientM (Bloc.postUsersUser (admin^.adminUsername) postUsersUserRequest) clientEnv
  where
    message :: ServantError -> MigrationError
    message = BlocError . T.pack . show

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
  parseJSON v = fail $ "Needed an object, found: " ++ show v

-- | This looks for any directory that contains a file with a
-- name that contains 'filename' and returns the found file,
-- but errors out if there are none or more thatn one.
findUniqueFile :: FilePath
               -> String
               -> ExceptT MigrationError IO FilePath
findUniqueFile path filename = do
  matches <- liftIO $ find always (contains filename) path
  case matches of
    [] -> throwError . FindContractError $
            "No Contracts matching filname: " <> T.pack filename
    [fp] -> return $ fp <> "/" <> filename
    _    -> throwError . FindContractError $
              "more than one match for: " <> T.pack filename

-- | searches for a file and grabs the source code.
grabSourceCode :: FilePath
               -> String
               -> ExceptT MigrationError IO Text
grabSourceCode path filename = findUniqueFile path filename >>= liftIO . T.readFile

isImportStatement :: Text -> Bool
isImportStatement = T.isPrefixOf "import"

-- | get all the dependencies for a solidity file.
grabImports :: Text -> [FilePath]
grabImports sourceCode =
  let ls = T.lines sourceCode
      imports = takeWhile isImportStatement ls
  in map (T.unpack . last . T.split ( == '/')) imports

type DependencyGraph = (G.Graph, G.Vertex -> (FilePath, FilePath, [FilePath]), FilePath -> Maybe G.Vertex)

data GraphState =
  GraphState { _edgeSet         :: Map FilePath (S.Set FilePath)
             , _visitedVertices :: S.Set FilePath
             }
makeLenses ''GraphState

-- | This get's the dependency graph for the files. Again, it rests on the assumption
-- that in any contracts project, there every contract filename is unique.
grabDependencyGraph :: FilePath
                    -> [FilePath]
                    -> ExceptT MigrationError IO DependencyGraph
grabDependencyGraph _main fps = do
    let initState = GraphState (Map.singleton _main (S.fromList fps)) (S.singleton _main)
    GraphState edges _ <- execStateT (grabDependencyGraph' fps) initState
    return . G.graphFromEdges . map (\(k,ns) -> (k, k, S.toList ns)) $ Map.toList edges
  where
    grabDependencyGraph' :: [FilePath]
                         -> StateT GraphState (ExceptT MigrationError IO) ()
    grabDependencyGraph' filepaths = do
      nbrs <- lift $ traverse grabNeighbors filepaths
      edgeSet %= \known -> foldr addEdges known nbrs
      visited <- use visitedVertices
      let visited' = foldr S.insert visited (map fst nbrs)
          nextVertices = S.toList $ S.unions (map snd nbrs) `S.difference` visited'
      case nextVertices of
        [] -> return ()
        _ -> do
          visitedVertices .= visited'
          grabDependencyGraph' nextVertices
    grabNeighbors :: FilePath -> ExceptT MigrationError IO (FilePath, S.Set FilePath)
    grabNeighbors filepath = do
      nbrs <- grabSourceCode "." filepath >>= return . grabImports
      return (filepath, S.fromList nbrs)
    addEdges :: (FilePath, S.Set FilePath) -> Map FilePath (S.Set FilePath) -> Map FilePath (S.Set FilePath)
    addEdges (v, ns) m = case Map.lookup v m of
      Nothing  -> Map.insert v ns m
      Just ns' -> Map.insert v (ns `S.union` ns') m

-- | Collect all the source code for a list of filenames.
readAndTrimFiles :: [FilePath] -> ExceptT MigrationError IO Text
readAndTrimFiles fps = do
    sources <- traverse (findUniqueFile "." >=>  fmap trimDependencies . liftIO . T.readFile) fps
    return . T.unlines . map T.strip $ sources

trimDependencies :: Text -> Text
trimDependencies = T.strip . T.unlines . dropWhile isImportStatement . T.lines

-- | replace the contract source file with the source code in Text form.
withSourceCode :: FilePath -- ^ contracts dir
               -> ContractForUpload 'AsFilename
               -> ExceptT MigrationError IO (ContractForUpload 'AsCode)
withSourceCode contractsDir c = do
  sourceCode <- grabSourceCode contractsDir $ c^.contractUploadSource
  let deps = grabImports sourceCode
  case deps of
    [] -> return $ c & contractUploadSource .~ sourceCode
    ds -> do
      let _main = c^.contractUploadSource
      g <- grabDependencyGraph _main ds
      let orderedFileIndices = (G.topSort (g^._1))
          vertexMapping = g^._2
          orderedFiles = reverse $ map (\v -> (vertexMapping v) ^._1) orderedFileIndices
      fullSource <- readAndTrimFiles orderedFiles
      return $ c & contractUploadSource .~ fullSource

--------------------------------------------------------------------------------
-- | Deploy Contracts
--------------------------------------------------------------------------------

data Contract =
  Contract { _contractName    :: Text
           , _contractAddress :: Address
           } deriving (Eq, Show)
makeLenses ''Contract

-- |  deploys a contract.
deployContract ::  ClientEnv
               -> AdminConfig
               -> Address -- ^ the admin's address
               -> ContractForUpload 'AsCode
               -> ExceptT MigrationError IO Address
deployContract env admin adminAddr contract =
  let req = PostUsersContractRequest
        { postuserscontractrequestSrc = contract^.contractUploadSource
        , postuserscontractrequestPassword = admin^.adminPassword
        , postuserscontractrequestContract = contract^.contractUploadName
        , postuserscontractrequestArgs = contract^.contractUploadInitialArgs
        , postuserscontractrequestTxParams = contract^.contractUploadTxParams
        , postuserscontractrequestValue = contract^.contractUploadNonce
        }
  in ExceptT $ do
       eresp <- runClientM (Bloc.postUsersContract (admin^.adminUsername) adminAddr req) env
       case eresp of
         Left serr -> do
           msg <- message serr
           return . Left $ msg
         Right success -> return . Right $ success
  where
    message :: ServantError -> IO MigrationError
    message e = do
      _ <- liftIO . putStrLn . show $ e
      return $ BlocError $ mconcat [ "Failed to deploy --a ["
                                   , contract ^. contractUploadName
                                   , "]-- "
                                   ]

-- | deploy all the contracts listed in the contracts.yaml file.
deployContracts :: ClientEnv
                -> AdminConfig
                -> Address -- ^ ownerAddress
                -> FilePath -- ^ location of contracts.yaml
                -> FilePath -- ^ contracts dir
                -> ExceptT MigrationError IO [Contract]
deployContracts env admin ownerAddr contractYaml contractsDir = do
  ecs <- liftIO $ Y.decodeFileEither contractYaml
  case ecs of
    Left e -> throwError . ParseError . T.pack . show $ e
    Right cs -> forM cs $ \c -> do
      c' <- withSourceCode contractsDir c
      addr <- deployContract env admin ownerAddr c'
      return $ Contract (c^.contractUploadName) addr

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
runMigration :: ClientEnv
             -> AdminConfig
             -> FilePath -- ^ location of contracts.yaml
             -> FilePath -- ^ localtion of contracts dir
             -> IO (Either MigrationError MigrationResult)
runMigration env admin contractYaml contractsDir = runExceptT $ do
  adminAddress <- createAdmin env admin
  liftIO $ print ("Admin created! Deploying Contracts" :: String)
  cs <- deployContracts env admin adminAddress contractYaml contractsDir
  liftIO $ print ("Successfully Depployed Contracts" :: String)
  return $ MigrationResult adminAddress cs
