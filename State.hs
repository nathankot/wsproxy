module State
( Client
, ServerState
, newServerState
, addClient
, removeClient
, findAllByEmail
, Message (PushMessage, client, message)
, Messenger
) where

import Data.Text (Text)
import qualified Network.WebSockets as WS
import Control.Concurrent (MVar)

type Client = (Text, WS.Connection)
type ServerState = [Client]

type Messenger = MVar Message
data Message = PushMessage { client :: Client
                           , message :: Text
                           }

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient c clients = c:clients

removeClient :: Client -> ServerState -> ServerState
removeClient c = filter ((/= fst c) . fst)

findAllByEmail :: Text -> ServerState -> [Client]
findAllByEmail email = filter (\c -> email == fst c)
