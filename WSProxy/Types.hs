module Types
( Messenger
, Message (PushMessage, client, message)
, Client
, ServerState
) where

import Data.Text (Text)
import qualified Network.WebSockets as WS
import Control.Concurrent (MVar)

-- | A connected websocket client
type Client = (Text, WS.Connection)

-- | State, representing a list of connected websocket clients
type ServerState = [Client]

type Messenger = MVar Message

-- | Message for a specific client
data Message = PushMessage { client :: Client
                           , message :: Text
                           }
