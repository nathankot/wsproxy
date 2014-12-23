{-# LANGUAGE OverloadedStrings #-}

module Api
( application
) where

import State
import Web.Scotty
import Network.HTTP.Types
import Control.Concurrent.MVar

-- API Implementation {{{
application :: MVar ServerState -> ScottyM ()
application _ = get "/ping" $ do
    status status200
    text "pong"
-- }}}
