{-# LANGUAGE OverloadedStrings #-}

module WSProxy.Main
( main
, application
) where

import GHC.Conc
import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception                    (finally)
import Control.Monad                        (forever, unless, void)
import Control.Monad.IO.Class               (liftIO)
import Data.Maybe                           (fromMaybe)
import qualified Data.Text                  as T
import Network.HTTP.Types                   (status200, status400)
import System.Environment                   (lookupEnv)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.WebSockets
import qualified Network.Wai.Handler.Warp   as W
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
    h <- getEnvWithDefault "HOST" "0.0.0.0"
    s <- getEnvWithDefault "SERVER" ""
    application Environment { host = h
                            , port = p
                            , server = s
                            }

application :: Environment -> IO ()
application e = do
    putStrLn "Starting wsproxy with environment >>"
    print e
    -- Store state in MVar's
    s <- newMVar newClients
    m <- newEmptyMVar :: IO Messenger
    -- This is the layer that passes messages from server to client and vice-versa.
    void $ forkIO $ listenToMessenger m
    -- Fork a websockets server
    void $ forkIO $ WS.runServer (host e) (port e)
                  $ wsServer m s (server e)

    http <- scottyApp $ httpServer s m
    W.run (port e) $ websocketsOr (WS.ConnectionOptions $ return ()) (wsServer m s (server e)) http

wsServer :: Messenger -> MVar Clients -> Server -> WS.ServerApp
wsServer me s se p = do
    conn <- WS.acceptRequest p
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn :: IO T.Text
    unless (isConnection msg) $ fail "Bad use of protocol"
    WS.sendTextData conn ("Connection acknowledged" :: T.Text)
    let email = T.drop (T.length connectPrefix) msg
    let c = (email, conn)
    void $ finally (connect s c
           -- Start receiving messages.
           >> forever (WS.receiveData conn
           -- And forwarding them.
           >>= \m -> pushMessage ServerMessage { messenger = me
                                               , message = m, client = c
                                               , recipientServer = se }))
           (disconnect s c) -- Close connection on failure.

httpServer :: MVar Clients -> Messenger -> ScottyM ()
httpServer s m = do
    middleware logStdout
    get "/" $ status status200

    post "/push" $ do
      clients <- liftIO $ readMVar s
      email <- param "email" `rescue` const next
      msg <- param "message" `rescue` const next
      let recipients = findAllByEmail email clients
      _ <- liftIO $ mapM pushMessage [ClientMessage { client = c, message = msg, messenger = m } | c <- recipients]
      status status200
      text "Acknowledged"

    post "/push" $ do
      status status400
      text "This endpoint requires an email and message"


