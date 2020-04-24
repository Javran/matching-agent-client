{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  #-}
module Main (main) where

import Data.ProtoLens
import Lens.Micro
import Network.Socket hiding (recv)
import System.Environment

import qualified Data.ByteString as BS
import qualified Proto.MatchingAgent as MA
import qualified Proto.MatchingAgent_Fields as MA

import MatchingAgent.Client.Core

main :: IO ()
main = do
  [fp] <- getArgs
  raw <- BS.readFile fp
  let hints = defaultHints { addrSocketType = Stream }
      req :: MA.FindTagRequest
      req = defMessage & MA.payload .~ raw
  addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "17151")
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  sendProto sock req
  msg <- recvProto @MA.FindTagResponse sock
  putStrLn . showMessage $ msg
  close sock
  pure ()
