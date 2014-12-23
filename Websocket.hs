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

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn :: IO Text

    if connectPrefix `T.isPrefixOf` msg then
      let email = T.drop (T.length connectPrefix) msg
          client = (email, conn)
      in finally (connect state client) (disconnect state client)
    else
      WS.sendTextData conn $ T.pack "Bad use of protocol"

    return ()

connect :: MVar ServerState -> Client -> IO ()
connect state client = do
  let conn = snd client

  -- First add the client to our state
  _ <- modifyMVar state $ \s -> do
    let s' = addClient client s
    return (s', s')

  _ <- forever $ do
    msg <- WS.receiveData conn :: IO Text
    WS.sendTextData conn msg

  return ()

disconnect :: MVar ServerState -> Client -> IO ()
disconnect state client = do

  _ <- modifyMVar state $ \s -> do
    let s' = removeClient client s
    return (s', s')

  return ()

-- }}}
