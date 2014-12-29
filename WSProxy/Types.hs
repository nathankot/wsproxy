module WSProxy.Types
( Messenger
, Message (PushMessage, client, message)
, Client
, Clients
) where

import Data.Text (Text)
import qualified Network.WebSockets as WS
import Control.Concurrent (MVar)

-- | A connected websocket client
type Client = (Text, WS.Connection)

-- | State, representing a list of connected websocket clients
type Clients = [Client]

type Messenger = MVar Message

-- | Message for a specific client
data Message = PushMessage { client :: Client
                           , message :: Text
                           }
