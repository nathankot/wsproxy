-- vim: foldmethod=marker

{-# LANGUAGE OverloadedStrings #-}

import State
import Web.Scotty
import Util
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Applicative
import Control.Monad.IO.Class
import qualified Websocket as SOCKET
import qualified Network.WebSockets as WS
import Network.HTTP.Types
import qualified Data.Text as T

main :: IO ()
main = do

  -- Store state in MVar's
  state <- newMVar newServerState
  messenger <- newEmptyMVar :: IO Messenger

  -- Find the env
  port <- read <$> getEnvWithDefault "PORT" "3636" :: IO Int
  websocketPort <- read <$> getEnvWithDefault "PORT" "9160" :: IO Int

  -- Fork a websockets server
  _ <- forkIO $ WS.runServer "0.0.0.0" websocketPort $ SOCKET.application state messenger

  -- Initialize scotty for our RESTFUL api
  scotty port $ do

    get "/ping" $ do
      status status200
      text "pong"

    get "/test" $ do
      clients <- liftIO $ readMVar state
      let email = "test@email.com"
      let c = findAllByEmail email clients
      _ <- send messenger "Test" c
      status status200
      text "Acknowledged"

send ::  MVar Message -> T.Text -> [Client] -> ActionM [()]
send messenger m clients = do
    let messages = map (\c -> PushMessage { client = c, message = m }) clients
    let sendMessage = liftIO . putMVar messenger
    if null messages
    then fail "No clients to send to"
    else sequence [sendMessage a | a <- messages]

