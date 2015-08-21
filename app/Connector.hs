{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, DoAndIfThenElse #-}

module Connector ( liftEither, makePeers) where

import qualified Peer as P (Peer, makePeer, showPeer, fromBsToInt) 
import qualified BencodeParser as BP (BEncode, annouce, infoHash, parseFromFile, parseFromBS, peers, piceSize, torrentSize, piecesHashSeq)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified UrlEncoder as Encoder (urlEncodeVars)
import qualified Data.Either as DE 
import qualified Network as N
import qualified Handshake as H 
import qualified Data.Traversable as T
import Data.Binary.Get
import Data.Word
import qualified System.IO as SIO
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP as HTTP
import Control.Concurrent.Async as Async (mapConcurrently)
import Control.Applicative
import Types

type TorrentContent = BP.BEncode            
          
          
          
logMsg a = liftIO $ print a          
          
          
          
makePeers :: String ->Int -> ExceptT String IO [P.Peer]  
makePeers tracker numberOfP = 
    do torrentContent <-  BP.parseFromFile tracker
       info@(numberOfPieces, maxP, maxLast) <- liftEither $ getSizeInfo torrentContent           
       globalStatus    <- liftIO $ newGlobalBitField numberOfPieces     
       ipsAndPorts <- peersIpAndPortsFromTracker torrentContent         
       infoHash <- liftEither $ BC.pack <$> BP.infoHash torrentContent
       logMsg $ "Number of avaliable peers " ++ show (length ipsAndPorts) 
       handshakes <- liftIO $ getHandshakes infoHash (take numberOfP ipsAndPorts)
       let (errorHandshakes, correctHanshakes) = DE.partitionEithers handshakes                                     
       logMsg $ "Handshakes with Error "++ show errorHandshakes
       logMsg $ "Correct Handshakes "++ show correctHanshakes
       
       -- ----                    
       
       piecesHash  <- liftEither $ BP.piecesHashSeq torrentContent                         
       let peers = mapM (\(handler, handshake) -> P.makePeer handler (H.peerName handshake) info globalStatus piecesHash) correctHanshakes
       liftIO peers

       
getHandshakes :: BC.ByteString -> [(N.HostName, N.PortNumber)] -> IO ([(Either String (SIO.Handle, H.Handshake))])     
getHandshakes infoHash ipsAndPorts = 
    liftIO $ Async.mapConcurrently (\(host,port) -> H.getHandshakes infoHash host port) ipsAndPorts  

 
 
type NumberOfPieces = Int
type NormalPieceSize = Int
type LastPieceSize = Int


getSizeInfo :: TorrentContent -> Either String (NumberOfPieces, NormalPieceSize, LastPieceSize)
getSizeInfo torrentContent = 
    do pieceSize   <- BP.piceSize torrentContent
       torrentSize <- BP.torrentSize torrentContent           
       let info@(nP, maxP, maxLast) = getSizeData torrentSize pieceSize  
       return info
    where
          getSizeData torrentSize pieceSize = 
               let tSize = fromIntegral torrentSize
                   pSize = fromIntegral pieceSize
                   numberOfPieces = ceiling $ tSize / pSize
                   lastPSize = tSize `mod` pSize
               in (numberOfPieces, pSize, lastPSize)
  

  
peersIpAndPortsFromTracker :: TorrentContent -> ExceptT String IO [(N.HostName, N.PortNumber)]
peersIpAndPortsFromTracker torrentContent = 
    do urlTracker  <- liftEither $ trackerUrl torrentContent
       resp <- liftIO . getResponseFromTracker $ urlTracker
       peersBS <- liftEither $ (BP.parseFromBS . BC.pack $ resp) >>= BP.peers
       let ipsAndPorts =  getIPandPort peersBS
       return ipsAndPorts

       
       
type TrackerResponse = String       
type URL = String                             
  
  
getResponseFromTracker :: URL -> IO TrackerResponse
getResponseFromTracker url = HTTP.simpleHTTP (HTTP.getRequest url) 
                             >>= HTTP.getResponseBody 
                             

trackerUrl :: TorrentContent -> Either String URL
trackerUrl fromDic = 
         do ann <- BP.annouce fromDic
            vars <- encodedVars <$> (BP.infoHash fromDic)
            return $ ann++"?"++vars
         where 
               encodedVars hash = 
                Encoder.urlEncodeVars [("info_hash", hash),
                                       ("peer_id", H.myId),
                                       ("left", "1000000000"),
                                       ("port", "6881"),
                                       ("compact", "1"),
                                       ("uploaded", "0"),
                                       ("downloaded", "0"),
                                       ("event", "started")]         

                
                                 
getIPandPort :: B.ByteString -> [(N.HostName, N.PortNumber)]                                                                                               
getIPandPort bs = runGet get32and16b (BL.fromChunks [bs])                                                                                             
         where 
               get32and16b :: Get [(N.HostName, N.PortNumber)]
               get32and16b = do empty <- isEmpty
                                if empty
                                then return [] 
                                else do ip <- show <$> getWord32be
                                        port <- fromIntegral <$> getWord16be
                                        rest <- get32and16b
                                        return $ (ip, port):rest
             
                         

    