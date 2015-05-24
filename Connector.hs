{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, DoAndIfThenElse #-}

module Connector ( liftEither, makePeers) where

import qualified Peer as P (Peer, makePeer, showPeer, fromBsToInt) 
import qualified BencodeParser as BP (BEncode, annouce, infoHash, parseFromFile, parseFromBS, peers, piceSize, torrentSize)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified UrlEncoder as Encoder (urlEncodeVars)
import qualified Data.Either as DE 
import qualified Network as N
import qualified Data.ByteString as B
import qualified System.IO as SIO
-- import qualified Data.ByteString.Base16 as B16
import Data.Binary.Get
import Data.Word
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP as HTTP
import Control.Concurrent.Async as Async (mapConcurrently)
import Control.Exception as E
import Control.Applicative
import Control.Monad.Error
import Types
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put


myId = "-TR2840-d0p22uiake0b" 
protocol = "BitTorrent protocol"

liftEither = ErrorT . return

                      
makePeers :: String ->Int -> ErrorT String IO [P.Peer]  
makePeers tracker numberOfP = do torrentContent <-  liftIO $ BP.parseFromFile tracker
                                 infoHash <- liftEither $ BC.pack<$>(torrentContent >>= BP.infoHash)
                                 urlTracker <-   liftEither $ torrentContent >>= trackerUrl
                                 pieceSize<- liftEither $ torrentContent >>= BP.piceSize
                                 torrentSize<- liftEither $ torrentContent >>= BP.torrentSize
                                 let numberOfPieces= ceiling $ ((fromIntegral torrentSize)/(fromIntegral pieceSize)) 
                                 resp <- (liftIO . getResponseFromTracker) urlTracker
                                 peersBS <- liftEither $ ((BP.parseFromBS . BC.pack) resp)  >>= BP.peers
                                 let ipsAndPorts =  getIPandPort peersBS    
                                 globalStatus <- liftIO $ newGlobalBitField numberOfPieces
                                 handshakes <- liftIO $ Async.mapConcurrently (\(host,port)->getHandshakes infoHash host port) (take numberOfP ipsAndPorts)
                                 let (errorHandshakes, correctHanshakes) = DE.partitionEithers handshakes
                                 liftIO $ print errorHandshakes
                                 let peers = map (\(handler, handshake) -> P.makePeer handler (peerName handshake) numberOfPieces globalStatus) correctHanshakes
                                 liftIO $ sequence peers
 
                             
getResponseFromTracker :: String -> IO String
getResponseFromTracker url = HTTP.simpleHTTP (HTTP.getRequest url) >>= HTTP.getResponseBody 

trackerUrl :: BP.BEncode -> Either String String
trackerUrl fromDic = do ann <- BP.annouce fromDic
                        vars <- encodedVars <$> (BP.infoHash fromDic)
                        return $ ann++"?"++vars
     where encodedVars hash = Encoder.urlEncodeVars [("info_hash", hash),
                                                     ("peer_id", myId),
                                                     ("left", "1000000000"),
                                                     ("port", "6881"),
                                                     ("compact", "1"),
                                                     ("uploaded", "0"),
                                                     ("downloaded", "0"),
                                                     ("event", "started")]         

                 
getIPandPort :: B.ByteString -> [(N.HostName, N.PortNumber)]                                                                                               
getIPandPort bs = runGet get32and16b (BL.fromChunks [bs])                                                                                             
       where get32and16b :: Get [(N.HostName, N.PortNumber)]
             get32and16b = do empty <- isEmpty
                              if empty
                              then return [] 
                              else do ip <- show <$> getWord32be
                                      port <- fromIntegral <$> getWord16be
                                      rest <- get32and16b
                                      return $ (ip, port):rest
             
             
-- HANDSHAKE --------------------------             

data Handshake = Handshake { len :: Int 
                           , peerProtocol :: String
                           , reserved :: B.ByteString
                           , hash :: B.ByteString
                           , peerName :: String
                           }              
  
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
                                     (\(e::SomeException) -> return . Left $ (show e) ++" "++ (show host))              
           
handshakes:: B.ByteString -> N.HostName -> N.PortNumber -> IO (SIO.Handle, Handshake)           
handshakes hash host port = do handle<- N.connectTo host (N.PortNumber  port)
                               SIO.hSetBinaryMode handle True
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
                                                                      