module Websocket
( application
) where

import State
import Control.Concurrent.MVar
import Control.Monad (forever)
import Control.Exception (finally)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

-- Sockets Implementation {{{

connectPrefix :: Text
connectPrefix = T.pack "Connect:"

application :: MVar ServerState -> Messenger -> WS.ServerApp
application state messenger pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn :: IO Text
    if isConnection msg then
      let email = T.drop (T.length connectPrefix) msg
          c = (email, conn)
      in flip finally (disconnect state c) $ do
        _ <- connect state c
        listenToMessenger messenger
    else
      WS.sendTextData conn $ T.pack "Bad use of protocol"

    return ()

listenToMessenger :: Messenger -> IO ()
listenToMessenger messenger = forever $ do
  instruction <- takeMVar messenger
  execute instruction
  return ()

execute :: Message -> IO()
execute (PushMessage { client = (_, conn), message = m }) = WS.sendTextData conn m

isConnection :: Text -> Bool
isConnection = T.isPrefixOf connectPrefix

connect :: MVar ServerState -> Client -> IO ServerState
connect state c = modifyMVar state $ \s -> do
  let s' = addClient c s
  return (s', s')

disconnect :: MVar ServerState -> Client -> IO ServerState
disconnect state c = modifyMVar state $ \s -> do
  let s' = removeClient c s
  return (s', s')

-- }}}
