{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  #-}
module Main (main) where

import MatchingAgent.Server
import System.Environment

import qualified Data.ByteString as BS

main :: IO ()
main = do
  binPath <- getEnv "MA_SERVER_BIN_PATH"
  port <- read <$> getEnv "MA_SERVER_PORT"
  patternBase <- getEnv "MA_SERVER_PATTERN_BASE"
  let serverConfig = ServerConfig binPath port patternBase
  [fp] <- getArgs
  raw <- BS.readFile fp
  withServer serverConfig $ \server -> do
    result <- findTag server raw
    print result
