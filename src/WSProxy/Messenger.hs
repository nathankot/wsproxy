module WSProxy.Messenger
( Messenger
, Message (ClientMessage, ServerMessage, client, message)
, listenToMessenger
, pushClientMessage
, pushServerMessage
) where

import Control.Concurrent.MVar (takeMVar, putMVar, MVar)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

import Web.Scotty (ActionM)
import qualified Network.WebSockets as WS

import WSProxy.Types

execute :: Message -> IO()
execute (ClientMessage { client = (_, conn), message = m }) = WS.sendTextData conn m
execute (ServerMessage { client = (email, _), message = m }) = return ()

listenToMessenger :: Messenger -> Server -> IO ()
listenToMessenger messenger _ = forever $ do
  instruction <- takeMVar messenger
  execute instruction
  return ()

pushClientMessage :: Messenger -> T.Text -> Clients -> ActionM [()]
pushClientMessage messenger m clients = do
    let messages = map (\c -> ClientMessage { client = c, message = m }) clients
    let send = liftIO . putMVar messenger
    if null messages
    then fail "No clients to send to"
    else sequence [send a | a <- messages]

pushServerMessage :: Messenger -> T.Text -> Clients -> IO ()
pushServerMessage messenger m clients = do
    return ()
