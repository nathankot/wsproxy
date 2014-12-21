-- vim: foldmethod=marker

{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Network.WebSockets as WS
import Web.Scotty
import Network.HTTP.Types
import Util
import Control.Concurrent.MVar

main :: IO ()
main = do
  -- Initialize scotty for our RESTFUL api
  port <- getEnvWithDefault "PORT" "3636"
  scotty (read port) api

  -- Initialize the websockets server
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" 9160 $ application state

-- API Implementation
-- {{{
api :: ScottyM ()
api = get "/ping" $ do
  status status200
  text "back"
-- }}}

-- Sockets Implementation
-- {{{
type Client = (Text, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client:clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
-- }}}
