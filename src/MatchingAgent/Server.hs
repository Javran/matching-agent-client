{-# LANGUAGE
    LambdaCase
  , NamedFieldPuns
  , TypeApplications
  #-}
module MatchingAgent.Server
  ( ServerConfig(..)
  , withServer
  , findTag
  , ServerHandle
  ) where

import Control.Concurrent
import Control.Exception.Safe
import Data.Function
import Data.ProtoLens
import Data.Tuple
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
    { scBinPath :: FilePath
    , scPort :: Int
    , scPatternBase :: FilePath
    }

data ServerState
  = ServerState
    { ssHandle :: ProcessHandle
    , ssPort :: Int
    , ssSock :: Maybe Socket
    }

newtype ServerHandle = ServerHandle (MVar ServerState)

startServer :: ServerConfig -> IO ServerState
startServer ServerConfig{scBinPath, scPort, scPatternBase} = do
  let crProc =
        (proc
          scBinPath
          [ "--port", show scPort
          , "--pattern_base", scPatternBase
          ]) { std_in = CreatePipe
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
ensureSock ss@ServerState{ssHandle = ph, ssSock = m, ssPort} =
  case m of
    Nothing -> do
      let hints = defaultHints { addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show ssPort)
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      fix (\retry count ->
        if count > 20
          then
            error "Failed to establish connection, no more retries."
          else do
            result <- tryIO $ do
              threadDelay $ 20 * 1000
              connect sock $ addrAddress addr
            case result of
              Left _ ->
                -- connection failure could be a result of binary
                -- exiting unexpectedly, in which case we should not retry again.
                getProcessExitCode ph >>= \case
                  Nothing -> retry (succ count)
                  Just ec ->
                    error $ "Unexpected exiting, code=" <> show ec
              Right _ -> pure ()
          ) (0 :: Int)
      pure $ ss {ssSock = Just sock}
    Just _ -> pure ss

findTagAux :: ServerState -> BS.ByteString -> IO ((T.Text, Float), ServerState)
findTagAux ssPre raw = do
  ss@ServerState{ssSock = Just sock} <- ensureSock ssPre
  let req :: MA.FindTagRequest
      req = defMessage & MA.payload .~ raw
  sendProto sock req
  msg <- recvProto @MA.FindTagResponse sock
  pure ((msg ^. MA.tag, msg ^. MA.result), ss)

withServer :: ServerConfig -> (ServerHandle -> IO a) -> IO a
withServer sc = bracket svOpen svClose
  where
    svOpen :: IO ServerHandle
    svOpen = startServer sc >>= fmap ServerHandle . newMVar
    svClose (ServerHandle mr) = do
      -- the waiting here is safe since this action is executed only once
      -- after all "findTag" calls are processed.
      ServerState {ssHandle = ph, ssSock = mSock} <- takeMVar mr
      case mSock of
        Nothing -> pure ()
        Just s -> close s
      terminateProcess ph

findTag :: ServerHandle -> BS.ByteString -> IO (T.Text, Float)
findTag (ServerHandle sh) raw =
  modifyMVar sh $ \ssPre -> swap <$> findTagAux ssPre raw
