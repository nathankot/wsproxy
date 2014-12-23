module State
( Client
, ServerState
, newServerState
, addClient
, removeClient
) where

import Data.Text (Text)
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client:clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)
