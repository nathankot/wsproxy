module WSProxy.Types
( Messenger
, Message (PushMessage, PullMessage, client, message)
, Client
, Clients
) where

import Control.Concurrent (MVar)
import Data.Text (Text)

import qualified Network.WebSockets as WS

-- | A connected websocket client
type Client = (Text, WS.Connection)

-- | State, representing a list of connected websocket clients
type Clients = [Client]

type Messenger = MVar Message

-- | Message for a specific client
data Message = PushMessage { client :: Client
                           , message :: Text
                           }
             | PullMessage { client :: Client
                           , message :: Text
                           }
