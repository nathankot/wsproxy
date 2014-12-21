{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified App
import Util

main :: IO ()
main = do
  port <- getEnvWithDefault "PORT" "3636"
  scotty (read port) App.api

