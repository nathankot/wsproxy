{-# LANGUAGE OverloadedStrings #-}

module WSProxy.Main
( main
, application
) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Types (status200, status400)
import System.Environment (lookupEnv)

import Network.Wai.Middleware.RequestLogger
import Web.Scotty
import qualified Network.WebSockets as WS

import WSProxy.Client
import WSProxy.Messenger

getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault name defaultValue = do
    x <- lookupEnv name
    return $ fromMaybe defaultValue x

main :: IO ()
main = do
  port <- read <$> getEnvWithDefault "PORT" "3636" :: IO Int
  websocketPort <- read <$> getEnvWithDefault "WEBSOCKET_PORT" "9160" :: IO Int
  host <- getEnvWithDefault "HOST" "0.0.0.0"
  application host port websocketPort

application :: String -> Int -> Int -> IO ()
application host port websocketPort = do
  -- Store state in MVar's
  state <- newMVar newClients
  messenger <- newEmptyMVar :: IO Messenger
  -- This is the layer that passes messages from server to
  -- client and vice-versa.
  _ <- (forkIO $ listenToMessenger messenger)
  -- Fork a websockets server
  putStrLn $ "Websocket listening on port " ++ show websocketPort
  _ <- forkIO $ WS.runServer host websocketPort $ wsServer state messenger
  -- Initialize scotty for our RESTFUL api
  putStrLn $ "REST API listening on port " ++ show port
  scotty port $ httpServer state messenger

wsServer :: MVar Clients -> Messenger -> WS.PendingConnection -> IO ()
wsServer state messenger pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn :: IO T.Text
    if isConnection msg then
      let email = T.drop (T.length connectPrefix) msg
          c = (email, conn)
      in flip finally (disconnect state c) $ do
        _ <- connect state c
        -- take messages and send them to the server
        forever $ do
          m <- WS.receiveData conn :: IO T.Text
          pullMessage messenger m [c]
    else
      WS.sendTextData conn $ T.pack "Bad use of protocol"
    return ()

httpServer :: MVar Clients -> Messenger -> ScottyM ()
httpServer state messenger = do
    middleware logStdout

    post "/push" $ do
      clients <- liftIO $ readMVar state
      email <- param "email" `rescue` const next :: ActionM T.Text
      msg <- param "message" `rescue` const next :: ActionM T.Text
      _ <- pushMessage messenger msg $ findAllByEmail email clients
      status status200
      text "Acknowledged"

    post "/push" $ do
      status status400
      text "This endpoint requires an email and message"
