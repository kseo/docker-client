{-# LANGUAGE RecordWildCards #-}

module Docker.Client.Client
  ( ping
  , version
  , ContainerListOptions(..)
  , defaultContainerListOptions
  , containerList
  , containerStart
  , newUnixSocketManager
  ) where

import Control.Monad (void)
import Data.Text (Text)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings, managerRawConnection)
import Network.HTTP.Client.Internal (makeConnection)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import Prelude hiding (all)
import Servant.API
import Servant.Client

import Docker.Client.Api (apiV1_25)
import Docker.Client.Types

newUnixSocketManager :: FilePath -> IO Manager
newUnixSocketManager path = do
  let mSettings = defaultManagerSettings { managerRawConnection = return $ openUnixSocket path }
  newManager mSettings
  where
    openUnixSocket filePath _ _ _ = do
      s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect s (S.SockAddrUnix filePath)
      makeConnection (SBS.recv s 8096)
                     (SBS.sendAll s)
                     (S.close s)

containerList :: ContainerListOptions -> ClientM [Container]
containerList opt = containerList' (containerListOptionAll opt)
                                   (containerListOptionLimit opt)
                                   (containerListOptionSize opt)
                                   (containerListOptionFilters opt)

containerCreate :: ContainerConfig -> HostConfig -> NetworkingConfig -> Text -> ClientM ContainerID
containerCreate config hostConfig networkingConfig containerName= do
  res <- containerCreate' ContainerCreateOptions {..} (Just containerName)
  return (containerCreateCreatedBodyID res)

containerStart :: ContainerID -> ClientM ()
containerStart = void . containerStart'

ping :: ClientM Text
version :: ClientM Version
containerList' :: Maybe Bool -> Maybe Int -> Maybe Bool -> Maybe Text -> ClientM [Container]
containerCreate' :: ContainerCreateOptions -> Maybe Text -> ClientM ContainerCreateCreatedBody
containerStart' :: ContainerID -> ClientM NoContent

ping
  :<|> version
  :<|> containerList'
  :<|> containerCreate'
  :<|> containerStart' = client apiV1_25
