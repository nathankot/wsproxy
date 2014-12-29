-- vim: foldmethod=marker

{-# LANGUAGE OverloadedStrings #-}

module WSProxy.Main (main) where

import WSProxy.Client
import WSProxy.Messenger
import Web.Scotty
import Control.Concurrent.MVar (newMVar, newEmptyMVar, readMVar)
import Control.Concurrent (forkIO)
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types (status200, status400)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Control.Exception (finally)
import qualified Network.WebSockets as WS
import qualified Data.Text as T

getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault name defaultValue = do
    x <- lookupEnv name
    return $ fromMaybe defaultValue x

main :: IO ()
main = do

  -- Store state in MVar's
  state <- newMVar newClients
  messenger <- newEmptyMVar :: IO Messenger

  -- Find the env
  port <- read <$> getEnvWithDefault "PORT" "3636" :: IO Int
  websocketPort <- read <$> getEnvWithDefault "WEBSOCKET_PORT" "9160" :: IO Int
  host <- getEnvWithDefault "HOST" "0.0.0.0"

  -- Fork a websockets server
  putStrLn $ "Websocket listening on port " ++ show websocketPort
  _ <- forkIO $ WS.runServer host websocketPort $ \pending -> do
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
  putStrLn $ "REST API listening on port " ++ show port
  scotty port $ do

    get "/ping" $ do
      status status200
      text "pong"

    post "/push" $ do
      clients <- liftIO $ readMVar state
      email <- param "email" `rescue` const next :: ActionM T.Text
      msg <- param "message" `rescue` const next :: ActionM T.Text
      _ <- sendMessage messenger msg $ findAllByEmail email clients
      status status200
      text "Acknowledged"

    post "/push" $ do
      status status400
      text "This endpoint requires an email and message"

