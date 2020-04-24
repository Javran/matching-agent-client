{-# LANGUAGE
    NamedFieldPuns
  , TypeApplications
  #-}
module MatchingAgent.Server where

import Data.ProtoLens
import Lens.Micro
import Network.Socket
import System.Process

import qualified Data.ByteString as BS
import qualified Proto.MatchingAgent as MA
import qualified Proto.MatchingAgent_Fields as MA
import qualified Data.Text as T

import MatchingAgent.Client.Core

data ServerConfig
  = ServerConfig
    { scServerBinPath :: FilePath
    , scPort :: Int
    , scPatternBase :: FilePath
    }

data ServerState
  = ServerState
    { ssHandle :: ProcessHandle
    , ssPort :: Int
    , ssSock :: Maybe Socket
    }

startServer :: ServerConfig -> IO ServerState
startServer ServerConfig{scServerBinPath, scPort, scPatternBase} = do
  let crProc =
        (proc
          scServerBinPath
          [ "--port", show scPort
          , "--pattern_base", scPatternBase
          ]) { std_in = NoStream
             , std_out = CreatePipe
             , std_err = CreatePipe
             }
  (_, _, _, ph) <- createProcess crProc
  pure $ ServerState
    { ssHandle = ph
    , ssPort = scPort
    , ssSock = Nothing
    }

ensureSock :: ServerState -> IO ServerState
ensureSock ss@ServerState{ssSock = m, ssPort} =
  case m of
    Nothing -> do
      let hints = defaultHints { addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show ssPort)
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      pure $ ss {ssSock = Just sock}
    Just _ -> pure ss

findTag :: ServerState -> BS.ByteString -> IO ((T.Text, Float), ServerState)
findTag ssPre raw = do
  ss@ServerState{ssSock = Just sock} <- ensureSock ssPre
  let req :: MA.FindTagRequest
      req = defMessage & MA.payload .~ raw
  sendProto sock req
  msg <- recvProto @MA.FindTagResponse sock
  pure ((msg ^. MA.tag, msg ^. MA.result), ss)
