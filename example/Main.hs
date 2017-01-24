module Main where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Options.Applicative
import Servant.Client (ClientM, ClientEnv(..), BaseUrl(..), Scheme(..), runClientM)

import Docker.Client.Client
import Docker.Client.Types

query :: ClientM [Container]
query = do
  ok <- ping
  liftIO $  print ok
  version <- version
  liftIO $ print (versionVersion version)
  containerList defaultContainerListOptions

main :: IO ()
main = join . execParser $
  info (helper <*> parser)
  (  fullDesc
  <> header "General program title/description"
  <> progDesc "What does this thing do?"
  )
  where
  parser :: Parser (IO ())
  parser = app
    <$> strOption
      (  long "host"
      <> short 'h'
      <> metavar "HOST"
      <> help "host"
      )
    <*> option auto
    (  long "port"
    <> short 'p'
    <> metavar "PORT"
    <> help "port"
    <> value 2376
    <> showDefault
    )

app :: String -> Int -> IO ()
app host port = do
  manager <- newUnixSocketManager "/var/run/docker.sock"
  res <- runClientM query (ClientEnv manager (BaseUrl Http host port ""))
  case res of
    Left err          -> putStrLn $ "Error: " ++ show err
    Right containers  -> mapM_ print containers
