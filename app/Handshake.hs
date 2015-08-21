{-# LANGUAGE ScopedTypeVariables #-}
module Handshake where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified System.IO as SIO
import qualified Network as N
import Control.Exception as E
import qualified Peer as P (Peer, makePeer, showPeer, fromBsToInt)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative
import Control.Monad.Except

data Handshake = Handshake { len :: Int 
                           , peerProtocol :: String
                           , reserved :: B.ByteString
                           , hash :: B.ByteString
                           , peerName :: String
                           }              
  
instance Show Handshake where
  show h = peerName h  
  
myId = "-TR2840-d0p22uiake0b" 
protocol = "BitTorrent protocol"  
  
instance Binary Handshake where  
  
  put handshake = putWord8 (fromIntegral . length $ protocol) 
               >> (putByteString $ BC.pack protocol) 
               >> (putWord64be 0)
               >> (putByteString $ hash handshake)
               >> (putByteString $ BC.pack $ peerName handshake)
  
  
  get = do len <- (P.fromBsToInt <$> getByteString 1)
           ptr <- BC.unpack <$> getByteString len
           rsrv <- getByteString 8
           hash <- getByteString 20
           peer <- BC.unpack <$> getByteString 20 
           return $ Handshake len ptr rsrv hash peer                       


           
getHandshakes:: B.ByteString -> N.HostName -> N.PortNumber -> IO (Either String (SIO.Handle, Handshake))           
getHandshakes hash host port =  E.catch (liftM Right $ handshakes hash host port)
                                     (\(e::SomeException) -> return . Left $ ("EEE "++ (show e)) ++" "++ (show host))              
           
           
           
handshakes:: B.ByteString -> N.HostName -> N.PortNumber -> IO (SIO.Handle, Handshake)           
handshakes hash host port = do handle<- N.connectTo host (N.PortNumber  port)
                              -- SIO.hSetBinaryMode handle True
                               SIO.hSetBuffering handle SIO.NoBuffering
                               sendHandshake handle hash $ BC.pack myId
                               handshake <-recvHandshake handle            
                               return (handle, handshake)
           
           
           
sendHandshake :: SIO.Handle -> B.ByteString -> B.ByteString -> IO ()
sendHandshake handle hash peer = BL.hPutStr handle $ encode handshake 
  where handshake = Handshake len protocol rsrv hash myId
        len = length protocol
        rsrv = B.replicate 8 0            

{--        
recvHandshake :: SIO.Handle -> IO Handshake 
recvHandshake handle =  decode <$> (BL.hGetContents handle)

 --}

              
recvHandshake :: SIO.Handle -> IO Handshake
recvHandshake handle = do len <- P.fromBsToInt <$>  B.hGet handle 1
                          ptr <- BC.unpack <$> B.hGet handle len
                          rsrv <- B.hGet handle 8
                          hash <- B.hGet handle 20
                          peer <- BC.unpack <$> B.hGet handle 20 
                          return $ Handshake len ptr rsrv hash peer
                                                                  