module State
( Client
, ServerState
, newServerState
, addClient
, removeClient
, PushMessage
) where

import Data.Text (Text)
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)
type ServerState = [Client]

data PushMessage = PushMessage { client :: Client
                               , message :: Text
                               }

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient c clients = c:clients

removeClient :: Client -> ServerState -> ServerState
removeClient c = filter ((/= fst c) . fst)
