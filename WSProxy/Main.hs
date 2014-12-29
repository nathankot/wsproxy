-- vim: foldmethod=marker

{-# LANGUAGE OverloadedStrings #-}

import Types
import Web.Scotty
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Applicative
import Control.Monad.IO.Class
import Websocket
import qualified Network.WebSockets as WS
import Network.HTTP.Types
import qualified Data.Text as T
import Data.Maybe
import System.Environment
import Control.Exception (finally)

getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault name defaultValue = do
    x <- lookupEnv name
    return $ fromMaybe defaultValue x

main :: IO ()
main = do
  -- Store state in MVar's
  state <- newMVar newServerState
  messenger <- newEmptyMVar :: IO Messenger

  -- Find the env
  port <- read <$> getEnvWithDefault "PORT" "3636" :: IO Int
  websocketPort <- read <$> getEnvWithDefault "PORT" "9160" :: IO Int

  -- Fork a websockets server
  _ <- forkIO $ WS.runServer "0.0.0.0" websocketPort $ \pending -> do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn :: IO T.Text
    if isConnection msg then
      let email = T.drop (T.length connectPrefix) msg
          c = (email, conn)
      in flip finally (disconnect state c) $ do
        _ <- connect state c
        listenToMessenger messenger
    else
      WS.sendTextData conn $ T.pack "Bad use of protocol"

    return ()

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

