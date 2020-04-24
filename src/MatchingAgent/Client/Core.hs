{-# LANGUAGE
    TypeApplications
  , OverloadedStrings
  #-}
module MatchingAgent.Client.Core where

import Data.Word
import Network.Socket.ByteString (recv, sendAll)
import Network.Socket hiding (recv)
import Data.ProtoLens
import Data.Bits
import Data.Endian

import qualified Data.ByteString as BS

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
