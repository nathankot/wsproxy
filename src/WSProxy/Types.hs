module WSProxy.Types
( Environment ( Environment
              , host
              , port
              , websocketPort
              , server )
, Host
, Port
, WebsocketPort
, Server
, Messenger
, Client
, Clients
, Message ( ClientMessage
          , ServerMessage
          , client
          , message
          , messenger
          , recipientServer )
) where

import Control.Concurrent           (MVar)
import Data.Text                    (Text)
import qualified Network.WebSockets as WS

type Host = String
type Port = Int
type WebsocketPort = Port

-- | A server, needs to include the protocol and port with no
--   trailing slash.
--   e.g @https://api.example.com:8080@
type Server = String

data Environment = Environment { host          :: Host
                               , port          :: Port
                               , websocketPort :: WebsocketPort
                               , server        :: Server
                               } deriving (Show)

-- | A connected websocket client
type Client = (Text, WS.Connection)

-- | State, representing a list of connected websocket clients
type Clients = [Client]

-- | An 'MVar' for messages
type Messenger = MVar Message

-- | Message for a specific client
data Message = ClientMessage { client  :: Client
                             , message :: Text
                             , messenger :: Messenger
                             }
             | ServerMessage { client  :: Client
                             , message :: Text
                             , messenger :: Messenger
                             , recipientServer :: Server
                             }
