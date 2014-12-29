module WSProxy.Messenger
( Messenger
, Message (PushMessage, client, message)
, listenToMessenger
, sendMessage
) where

import Control.Concurrent.MVar (takeMVar, putMVar, MVar)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

import Web.Scotty (ActionM)
import qualified Network.WebSockets as WS

import WSProxy.Types

execute :: Message -> IO()
execute (PushMessage { client = (_, conn), message = m }) = WS.sendTextData conn m

listenToMessenger :: Messenger -> IO ()
listenToMessenger messenger = forever $ do
  instruction <- takeMVar messenger
  execute instruction
  return ()

sendMessage :: MVar Message -> T.Text -> [Client] -> ActionM [()]
sendMessage messenger m clients = do
    let messages = map (\c -> PushMessage { client = c, message = m }) clients
    let send = liftIO . putMVar messenger
    if null messages
    then fail "No clients to send to"
    else sequence [send a | a <- messages]

