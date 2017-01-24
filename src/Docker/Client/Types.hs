{-# LANGUAGE DeriveGeneric #-}

module Docker.Client.Types where

import Data.Aeson
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import qualified Data.List as DL
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHC.Generics
import Prelude hiding (all)
import Web.HttpApiData

newtype ImageID = ImageID Text
  deriving (Eq, Show, Generic)

instance ToJSON ImageID
instance FromJSON ImageID

newtype ContainerID = ContainerID Text
  deriving (Eq, Show, Generic)

instance ToJSON ContainerID
instance FromJSON ContainerID

instance ToHttpApiData ContainerID where
  toUrlPiece (ContainerID cid)    = toUrlPiece cid
  toHeader (ContainerID cid)      = toHeader cid
  toQueryParam (ContainerID cid)  = toQueryParam cid

data Version = Version
  { versionVersion       :: Text
  , versionApiVersion    :: Text
  , versionMinAPIVersion :: Text
  , versionGitCommit     :: Text
  , versionGoVersion     :: Text
  , versionOs            :: Text
  , versionArch          :: Text
  , versionKernelVersion :: Text
  , versionExperimental  :: Bool
  , versionBuildTime     :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Version where
  parseJSON = genericParseJSON opts
    where opts = defaultOptions { fieldLabelModifier = stripPrefix "version" }

data Container = Container
  { containerId               :: ContainerID
  , containerNames            :: [Text]
  , containerImage            :: Text
  , containerImageID          :: ImageID
  , containerCommand          :: Text
  , containerCreated          :: Int
  -- FIXME: Add Ports
  , containerSizeRw           :: Maybe Int
  , containerSizeRootFs       :: Maybe Int
  -- FIXME: Add Labels
  , containerState            :: Text
  , containerStatus           :: Text
  -- FIXME: Add HostConfig
  -- FIXME: Add NetworkSettings
  -- FIXME: Add Mounts
  } deriving (Show, Eq, Generic)

instance FromJSON Container where
  parseJSON = genericParseJSON opts
    where opts = defaultOptions { fieldLabelModifier = stripPrefix "container" }

data ContainerListOptions = ContainerListOptions
  { containerListOptionAll     :: Maybe Bool
  , containerListOptionLimit   :: Maybe Int
  , containerListOptionSize    :: Maybe Bool
  , containerListOptionFilters :: Maybe Text
  } deriving (Eq, Show)

defaultContainerListOptions :: ContainerListOptions
defaultContainerListOptions = ContainerListOptions
  { containerListOptionAll     = Just False
  , containerListOptionLimit   = Nothing
  , containerListOptionSize    = Just False
  , containerListOptionFilters = Nothing
  }

data ContainerCreateCreatedBody = ContainerCreateCreatedBody
  { containerCreateCreatedBodyID :: ContainerID
  , containerCreateCreatedBodyWarnings :: [Text]
  } deriving (Eq, Show, Generic)

instance FromJSON ContainerCreateCreatedBody where
  parseJSON = genericParseJSON opts
    where opts = defaultOptions { fieldLabelModifier = stripPrefix "containerCreateCreatedBody" }

data ContainerConfig = ContainerConfig
  {
  } deriving (Eq, Show, Generic)

data HostConfig = HostConfig
  { hostConfigNetworkMode :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON HostConfig where
  parseJSON = genericParseJSON opts
    where opts = defaultOptions { fieldLabelModifier = stripPrefix "hostConfig" }

data NetworkingConfig = NetworkingConfig
  {
  } deriving (Eq, Show, Generic)

data ContainerCreateOptions = ContainerCreateOptions
  { containerCreateOptionConfig            :: ContainerConfig
  , containerCreateOptionHostConfig        :: HostConfig
  , containerCreateOptionNetworkingConfig  :: NetworkingConfig
  } deriving (Eq, Show, Generic)

stripPrefix :: String -> String -> String
stripPrefix prefix = fromJust . DL.stripPrefix prefix
