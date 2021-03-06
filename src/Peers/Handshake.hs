{-# LANGUAGE ScopedTypeVariables #-}
module Peers.Handshake where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Peers.Peer as P
import qualified Types as TP
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

data Handshake = Handshake { len :: Int
                           , peerProtocol :: String
                           , reserved :: B.ByteString
                           , hash :: B.ByteString
                           , peerName :: String
                           }

instance Show Handshake where
  show = peerName

myId = "-TR2840-d0p22uiake0b"
protocol = "BitTorrent protocol"

instance Binary Handshake where   -- Tetsts

  put handshake = putWord8 (fromIntegral . length $ protocol)
               >> putByteString (BC.pack protocol)
               >> putWord64be 0
               >> putByteString (hash handshake)
               >> putByteString (BC.pack $ peerName handshake)


  get = do
      len <- P.fromBsToInt <$> getByteString 1
      ptr <- BC.unpack <$> getByteString len
      rsrv <- getByteString 8
      hash <- getByteString 20
      peer <- BC.unpack <$> getByteString 20
      return $ Handshake len ptr rsrv hash peer


createHandshake :: B.ByteString -> B.ByteString
createHandshake hash = BL.toStrict . encode $ Handshake len protocol rsrv hash myId
    where  len = length protocol
           rsrv = B.replicate 8 0


decodeHandshake ::
  B.ByteString
  -> Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, Handshake)
decodeHandshake bs =  decodeOrFail $ BL.fromStrict bs


convertParsedOutput ::
  Either (BL.ByteString , ByteOffset, String) (BL.ByteString, ByteOffset, Handshake)
  -> TP.Perhaps (BC.ByteString, Handshake)
convertParsedOutput x =
  case x of
    Left (_, _, e) -> Left $ "Problem with parsing handshake " ++ show e
    Right (leftOver, _, h) ->  Right (BL.toStrict leftOver,  h)
