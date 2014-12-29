module WSProxy.Client
( Client
, Clients
, findAllByEmail
, newClients
, connectPrefix
, isConnection
, connect
, disconnect
, addClient
, removeClient
) where

import Control.Concurrent.MVar
import qualified Data.Text as T

import WSProxy.Types

-- Sockets Implementation {{{

connectPrefix :: T.Text
connectPrefix = T.pack "Connect:"

isConnection :: T.Text -> Bool
isConnection = T.isPrefixOf connectPrefix

connect :: MVar Clients -> Client -> IO Clients
connect state c = modifyMVar state $ \s -> do
  let s' = addClient c s
  return (s', s')

disconnect :: MVar Clients -> Client -> IO Clients
disconnect state c = modifyMVar state $ \s -> do
  let s' = removeClient c s
  return (s', s')

newClients :: Clients
newClients = []

addClient :: Client -> Clients -> Clients
addClient c clients = c:clients

removeClient :: Client -> Clients -> Clients
removeClient c = filter ((/= fst c) . fst)

findAllByEmail :: T.Text -> Clients -> [Client]
findAllByEmail email = filter (\c -> email == fst c)

-- }}}
