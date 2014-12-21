{-# LANGUAGE OverloadedStrings #-}

module App
( api
) where

import Web.Scotty
import Network.HTTP.Types

api :: ScottyM ()
api = get "/ping" $ do
  status status200
  text "back"
