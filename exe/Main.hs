{-# LANGUAGE
    OverloadedStrings
  #-}
module Main (main) where

import Control.Monad
import MatchingAgent.Server
import System.Environment

import qualified Data.ByteString as BS

main :: IO ()
main = do
  binPath <- getEnv "MA_SERVER_BIN_PATH"
  port <- read <$> getEnv "MA_SERVER_PORT"
  patternBase <- getEnv "MA_SERVER_PATTERN_BASE"
  let serverConfig = ServerConfig binPath port patternBase
  paths <- getArgs
  -- raw <- BS.readFile fp
  withServer serverConfig $ \server ->
    forM_ paths $ \fp -> do
      raw <- BS.readFile fp
      putStrLn $ "F: " <> fp
      result <- findTag server raw
      putStrLn $ "  -> " <> show result
