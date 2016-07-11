{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Tracker.Connector (makePeers) where

import qualified Peers.Peer as P
import qualified Bencode.BInfo as BP
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Tracker.UrlEncoder as Encoder (urlEncodeVars)
import qualified Data.Either as DE
import qualified Network as N
import qualified Peers.Handshake as H
import qualified Data.Traversable as T
import Data.Binary.Get
import Data.Word
import qualified System.IO as SIO
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP as HTTP
import Control.Concurrent.Async as Async (mapConcurrently)
import Control.Applicative
import qualified Types as TP
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Monad.STM
import Data.List
import qualified Tracker.UDPTracker as UDP

type TorrentContent = BP.BEncode


makePeers :: String -> TP.ExceptT String IO ([P.Peer], TP.SizeInfo, B.ByteString)
makePeers tracker =
  do torrentContent <- BP.parseFromFile tracker
     pSize          <- TP.liftEither $ BP.piceSize torrentContent
     pNLls          <- TP.liftEither $ BP.parsePathAndLenLs torrentContent
     let sizeInfo = BP.makeSizeInfo pNLls pSize
     TP.liftIO $ print "MK P"
     ipsAndPorts    <- getPeers torrentContent
     TP.liftIO $ print $ "DONE P " ++ (show $ length ipsAndPorts)

     infoHash       <- TP.liftEither $ BC.pack <$> BP.infoHash torrentContent
     pHashes        <- TP.liftEither $ BP.piecesHashSeq torrentContent

     let makePeer (host, p) =
          P.Peer { P.hostName     = host
                 , P.port         = (fromIntegral p)
                 , P.pieces       = []
                 , P.infoHash     = infoHash
                 , P.unChoked     = False
                 , P.buffer       = BC.empty
                 , P.pieceHashes  = pHashes
                 }

     let peers = map makePeer ipsAndPorts
     name <- TP.liftEither $ BP.torrentName torrentContent
     return (peers, sizeInfo, name)



getSizeInfo :: TorrentContent
            -> Either String TP.SizeInfo
getSizeInfo torrentContent =
  do pieceSize   <- BP.piceSize torrentContent
     torrentSize <- BP.torrentSize torrentContent
     return $ TP.getSizeData torrentSize pieceSize



getPeers :: TorrentContent -> TP.ExceptT String IO [(N.HostName, N.PortNumber)]
getPeers torrentContent =
    do ann     <- TP.liftEither $ BP.annouce torrentContent
       annType <- TP.liftEither $ BP.getAnnounce ann
       infoH   <- TP.liftEither $ BP.infoHash torrentContent

       case annType of
           BP.HTTP tracker -> do
               urlTracker <- TP.liftEither $ trackerUrl tracker infoH torrentContent
               TP.liftIO $ print urlTracker

               resp       <- TP.liftIO . getResponseFromTracker $ urlTracker
               parsedResp <- TP.liftEither $ (BP.parseFromBS . BC.pack $ resp)
               peersBS    <- TP.liftEither $ BP.peers parsedResp
               return $ getIPandPort peersBS

           BP.UDP st -> do
               (hn, port) <-  TP.liftEither $ BP.parseUDPAnnounce st
              -- TP.liftIO $ print conn
               UDP.getHostsAndIps (BC.unpack hn) (BC.unpack port) (BC.pack infoH)
              -- return hips -- $ error "Not supported"



type TrackerResponse = String
type URL = String


getResponseFromTracker :: URL -> IO TrackerResponse
getResponseFromTracker url =
    HTTP.simpleHTTP (HTTP.getRequest url)
    >>= HTTP.getResponseBody


trackerUrl :: BC.ByteString -> String -> TorrentContent -> Either String URL
trackerUrl ann infoHash romDic =
    do
       let vars =  encodedVars infoHash-- <$> f--(BP.infoHash fromDic)
       return $ (BC.unpack ann) ++"?"++vars
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
        get32and16b =
            do empty <- isEmpty
               if empty then
                   return []
               else do
                   ip <- show <$> getWord32be
                   port <- fromIntegral <$> getWord16be
                   rest <- get32and16b
                   return $ (ip, port):rest
