{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Tracker.Connector (makePeers) where

import qualified Peers.Peer as P
import qualified Data.ByteString as B
import qualified Bencode.BInfo as BI
import qualified Data.ByteString.Char8 as BC
import qualified Network as N
import Tracker.TrackerUtils (getIPandPort)
import qualified Types as TP
import qualified Tracker.UDPTracker as UDP_T
import qualified Tracker.HTTPTracker as HTTP_T


makePeers :: String
          -> TP.ExceptT String IO ([P.Peer], TP.SizeInfo, B.ByteString, [TP.FileInfo])
makePeers tracker = do
    torrentContent <- BI.parseFromFile tracker
    ipsAndPorts    <- getPeers torrentContent
    TP.liftEither $ createPeer torrentContent ipsAndPorts


createPeer :: BI.BEncode
     -> [(N.HostName, N.PortNumber)]
     -> Either String
              ([P.Peer], TP.SizeInfo, BC.ByteString, [TP.FileInfo])
createPeer torrentContent ipsAndPorts = do
    pSize    <- BI.piceSize torrentContent
    fInfos   <- BI.parsePathAndLenLs torrentContent
    infoHash <- BC.pack <$> BI.infoHash torrentContent
    pHashes  <- BI.piecesHashSeq torrentContent
    name     <- BI.torrentName torrentContent

    let makePeer (host, p) =
          P.Peer { P.hostName     = host
                 , P.port         = fromIntegral p
                 , P.pieces       = []
                 , P.infoHash     = infoHash
                 , P.unChoked     = False
                 , P.buffer       = BC.empty
                 , P.pieceHashes  = pHashes
                 }

    let sizeInfo = BI.makeSizeInfo fInfos pSize
    let peers = map makePeer ipsAndPorts

    return (peers, sizeInfo, name, fInfos)



getSizeInfo :: BI.BEncode
            -> Either String TP.SizeInfo
getSizeInfo torrentContent = do
    pieceSize   <- BI.piceSize torrentContent
    torrentSize <- BI.torrentSize torrentContent
    return $ TP.getSizeData torrentSize pieceSize


getPeers :: BI.BEncode
         -> TP.ExceptT String IO [(N.HostName, N.PortNumber)]
getPeers torrentContent = do
    ann     <- TP.liftEither $ BI.annouce torrentContent
    annType <- TP.liftEither $ BI.getAnnounce ann
    infoH   <- TP.liftEither $ BI.infoHash torrentContent
    case annType of
        BI.HTTP tracker ->
            HTTP_T.getHostsAndIps tracker infoH torrentContent

        BI.UDP tracker ->
            UDP_T.getHostsAndIps tracker infoH
