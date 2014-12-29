module Websocket
( findAllByEmail
, newServerState
, listenToMessenger
, connectPrefix
, isConnection
, connect
, disconnect
, addClient
, removeClient
) where

import Types
import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

-- Sockets Implementation {{{

connectPrefix :: Text
connectPrefix = T.pack "Connect:"

listenToMessenger :: Messenger -> IO ()
listenToMessenger messenger = forever $ do
  instruction <- takeMVar messenger
  execute instruction
  return ()

execute :: Message -> IO()
execute (PushMessage { client = (_, conn), message = m }) = WS.sendTextData conn m

isConnection :: Text -> Bool
isConnection = T.isPrefixOf connectPrefix

connect :: MVar ServerState -> Client -> IO ServerState
connect state c = modifyMVar state $ \s -> do
  let s' = addClient c s
  return (s', s')

disconnect :: MVar ServerState -> Client -> IO ServerState
disconnect state c = modifyMVar state $ \s -> do
  let s' = removeClient c s
  return (s', s')

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient c clients = c:clients

removeClient :: Client -> ServerState -> ServerState
removeClient c = filter ((/= fst c) . fst)

findAllByEmail :: Text -> ServerState -> [Client]
findAllByEmail email = filter (\c -> email == fst c)

-- }}}
