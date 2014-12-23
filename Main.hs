-- vim: foldmethod=marker

import State
import Web.Scotty
import Util
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Applicative
import qualified Api as API
import qualified Websocket as SOCKET
import qualified Network.WebSockets as WS

main :: IO ()
main = do

    -- Store state in an MVAR
    state <- newMVar newServerState

    -- Find the env
    port <- read <$> getEnvWithDefault "PORT" "3636" :: IO Int
    websocketPort <- read <$> getEnvWithDefault "PORT" "9160" :: IO Int

    -- Fork a websockets server
    _ <- forkIO $ WS.runServer "0.0.0.0" websocketPort $ SOCKET.application state

    -- Initialize scotty for our RESTFUL api
    scotty port $ API.application state

    -- Explicitly return nothing
    return ()

