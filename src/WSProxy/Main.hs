{-# LANGUAGE OverloadedStrings #-}

module WSProxy.Main
( main
, application
) where

import GHC.Conc
import Control.Applicative                  ((<$>))
import Control.Concurrent.MVar
import Control.Exception                    (finally)
import Control.Monad                        (forever, unless)
import Control.Monad.IO.Class               (liftIO)
import Data.Maybe                           (fromMaybe)
import qualified Data.Text                  as T
import Network.HTTP.Types                   (status200, status400)
import System.Environment                   (lookupEnv)
import Network.Wai.Middleware.RequestLogger
import Web.Scotty
import qualified Network.WebSockets         as WS
import WSProxy.Client
import WSProxy.Messenger
import WSProxy.Types

getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault name defaultValue = do
    x <- lookupEnv name
    return $ fromMaybe defaultValue x

main :: IO ()
main = do
  p <- read <$> getEnvWithDefault "PORT" "3636" :: IO Int
  wp <- read <$> getEnvWithDefault "WEBSOCKET_PORT" "9160" :: IO Int
  h <- getEnvWithDefault "HOST" "0.0.0.0"
  s <- getEnvWithDefault "SERVER" ""
  application Environment { host = h
                               , port = p
                               , websocketPort = wp
                               , server = s
                               }
application :: Environment -> IO ()
application environment = do
  putStrLn "Starting wsproxy with environment >>"
  putStrLn $ show environment
  -- Store state in MVar's
  state <- newMVar newClients
  messenger <- newEmptyMVar :: IO Messenger
  -- This is the layer that passes messages from server to client and vice-versa.
  _ <- forkIO $ listenToMessenger messenger $ server environment
  -- Fork a websockets server
  _ <- forkIO $ WS.runServer (host environment) (websocketPort environment) $ wsServer state messenger
  -- Initialize scotty for our RESTFUL api
  -- Scotty will hold the main thead open
  scotty (port environment) $ httpServer state messenger

wsServer :: MVar Clients -> Messenger -> WS.PendingConnection -> IO ()
wsServer state messenger pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn :: IO T.Text
    unless (isConnection msg) $ fail "Bad use of protocol"
    let email = T.drop (T.length connectPrefix) msg
    let c = (email, conn)
    finally (connect state c -- Connect.
              >> forever (WS.receiveData conn -- Start receiving messages.
              >>= \m -> pushServerMessage messenger m [c])) -- And forwarding them.
            (disconnect state c) >> return () -- Close connection on failure.

httpServer :: MVar Clients -> Messenger -> ScottyM ()
httpServer state messenger = do

    middleware logStdout

    post "/push" $ do
      clients <- liftIO $ readMVar state
      email <- param "email" `rescue` const next :: ActionM T.Text
      msg <- param "message" `rescue` const next :: ActionM T.Text
      _ <- pushClientMessage messenger msg $ findAllByEmail email clients
      status status200
      text "Acknowledged"

    post "/push" $ do
      status status400
      text "This endpoint requires an email and message"

