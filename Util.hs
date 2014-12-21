module Util
( getEnvWithDefault
) where

import Data.Maybe
import System.Environment

getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault name defaultValue = do
    x <- lookupEnv name
    return $ fromMaybe defaultValue x
