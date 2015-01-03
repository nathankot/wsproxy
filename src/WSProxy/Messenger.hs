module WSProxy.Messenger
( Messenger
, Message (ClientMessage, ServerMessage, client, message)
, listenToMessenger
, pushMessage
) where

import Control.Concurrent.MVar (takeMVar, putMVar)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

import Web.Scotty (ActionM)
import qualified Network.WebSockets as WS
import Network.HTTP.Conduit

import WSProxy.Types

execute :: Message -> IO()
execute (ClientMessage { client = (_, conn), message = m }) = WS.sendTextData conn m
execute (ServerMessage { client = (email, _), message = m }) = return ()

listenToMessenger :: Messenger -> Server -> IO ()
listenToMessenger messenger _ = forever $ do
  instruction <- takeMVar messenger
  execute instruction
  return ()

pushMessage :: Message -> IO ()
pushMessage m = putMVar (messenger m) m
