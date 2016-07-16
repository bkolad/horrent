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
    pSize          <- TP.liftEither $ BI.piceSize torrentContent
    fInfos         <- TP.liftEither $ BI.parsePathAndLenLs torrentContent
    let sizeInfo = BI.makeSizeInfo fInfos pSize
    ipsAndPorts    <- getPeers torrentContent

    infoHash       <- TP.liftEither $ BC.pack <$> BI.infoHash torrentContent
    pHashes        <- TP.liftEither $ BI.piecesHashSeq torrentContent

    let makePeer (host, p) =
          P.Peer { P.hostName     = host
                 , P.port         = fromIntegral p
                 , P.pieces       = []
                 , P.infoHash     = infoHash
                 , P.unChoked     = False
                 , P.buffer       = BC.empty
                 , P.pieceHashes  = pHashes
                 }

    let peers = map makePeer ipsAndPorts
    name <- TP.liftEither $ BI.torrentName torrentContent
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
