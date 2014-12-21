{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.HTTP.Types
import Util

main :: IO ()
main = do
  port <- getEnvWithDefault "PORT" "3636"
  scotty (read port) api

api :: ScottyM ()
api = get "/ping" $ do
  status status200
  text "back"
