{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import App (api)
import Util

main :: IO ()
main = do
  port <- getEnvWithDefault "PORT" "3636"
  scotty (read port) api

