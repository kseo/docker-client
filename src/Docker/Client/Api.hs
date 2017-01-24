{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Docker.Client.Api
  ( ApiV1_25
  , apiV1_25
  ) where

import Data.Proxy
import Data.Text (Text)
import Servant.API

import Docker.Client.Types

type Api = "_ping" :> Get '[PlainText] Text
      :<|> "version" :> Get '[JSON] Version
      :<|> "containers" :> "json" :> QueryParam "all" Bool
                                  :> QueryParam "limit" Int
                                  :> QueryParam "size" Bool
                                  :> QueryParam "filters" Text
                                  :> Get '[JSON] [Container]
      :<|> "containers" :> "create" :> ReqBody '[JSON] ContainerCreateOptions
                                    :> QueryParam "name" Text
                                    :> Get '[JSON] ContainerCreateCreatedBody
      :<|> "containers" :> Capture "id" ContainerID :> Get '[JSON] NoContent

type ApiV1_25 = "v1.25" :> Api

apiV1_25 :: Proxy Api
apiV1_25 = Proxy
