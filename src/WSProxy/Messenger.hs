{-# LANGUAGE OverloadedStrings #-}

module WSProxy.Messenger
( Messenger
, Message (ClientMessage, ServerMessage, client, message)
, listenToMessenger
, pushMessage
) where

import Control.Concurrent.MVar (takeMVar, putMVar)
import Control.Monad
import Control.Applicative
import qualified Data.Text as T

import qualified Network.WebSockets as WS
import Network.HTTP.Conduit
import Network (withSocketsDo)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)

import WSProxy.Types

execute :: Message -> IO ()
execute (ClientMessage { client = (_, conn), message = m }) = WS.sendTextData conn m
execute (ServerMessage { client = (e, _), message = m, recipientServer = s }) =
  unless (null s) $ withSocketsDo $ do
    r <- setQueryString (query e m) <$> parseUrl s
    _ <- withManager $ httpLbs r
    return ()

listenToMessenger :: Messenger -> IO ()
listenToMessenger m = forever $ takeMVar m >>= execute

pushMessage :: Message -> IO ()
pushMessage m = putMVar (messenger m) m

-- Internals

query :: T.Text -> T.Text -> [(ByteString, Maybe ByteString)]
query e m = [("email", Just e'), ("message", Just m')]
  where e' = encodeUtf8 e
        m' = encodeUtf8 m
