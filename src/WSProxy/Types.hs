module WSProxy.Types
( Environment (Environment, host, port, websocketPort, server)
, Host
, Port
, WebsocketPort
, Server
, Messenger
, Message (ClientMessage, ServerMessage, client, message)
, Client
, Clients
) where

import Control.Concurrent (MVar)
import Data.Text (Text)

import qualified Network.WebSockets as WS

type Host = String
type Port = Int
type WebsocketPort = Port
type Server = String

data Environment = Environment { host :: Host
                               , port :: Port
                               , websocketPort :: WebsocketPort
                               , server :: Server
                               }

-- | A connected websocket client
type Client = (Text, WS.Connection)

-- | State, representing a list of connected websocket clients
type Clients = [Client]

type Messenger = MVar Message

-- | Message for a specific client
data Message = ClientMessage { client :: Client
                             , message :: Text
                             }
             | ServerMessage { client :: Client
                             , message :: Text
                             }
