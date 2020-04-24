{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  #-}
module MaClientMain
  ( main
  ) where

import Data.Bits
import Data.Endian
import Data.ProtoLens
import Data.Word
import Lens.Micro
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import System.Environment

import qualified Data.ByteString as BS
import qualified Proto.MatchingAgent as MA
import qualified Proto.MatchingAgent_Fields as MA

{-
  TODO:
  - an unsafe core module
  - wrap it around and keep a ma_server running at runtime, and serve as
    IO action.
  - assumptions:
    + server doesn't crash (i.e. ProcessHandle is safe to reuse)
    + sockets however could be timed out so it's lazily created and used.
 -}

-- keep receiving until get the desired length.
recvAtLeast :: Socket -> Int -> IO BS.ByteString
recvAtLeast s todoCount = do
  payload <- recv s todoCount
  let remaining = todoCount - BS.length payload
  if payload == "" || remaining == 0
    then pure payload
    else (payload <>) <$> recvAtLeast s remaining

sendProto :: Message msg => Socket -> msg -> IO ()
sendProto sock msg = do
  let encoded = encodeMessage msg
      len :: Word32
      len = toLittleEndian . fromIntegral $ BS.length encoded
      encodedLen =
        BS.pack
        . fmap (fromIntegral @Word32 @Word8 . (.&. 0xFF) )
        $ [len, len `shiftR` 8, len `shiftR` 16, len `shiftR` 24]
  sendAll sock encodedLen
  sendAll sock encoded

recvProto :: Message msg => Socket -> IO msg
recvProto sock = do
  [r0, r1, r2, r3] <- fmap (fromIntegral @Word8 @Word32) . BS.unpack <$> recvAtLeast sock 4
  let responseSize =
        fromLittleEndian
        $ r0 .|. (r1 `shiftL` 8) .|. (r2 `shiftL` 16) .|. (r3 `shiftL` 24)
  responseRaw <- recvAtLeast sock (fromIntegral responseSize)
  case decodeMessage responseRaw of
    Left err -> error $ "decode error: " <> err
    Right msg -> pure msg

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
