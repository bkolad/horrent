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
import Data.Either (partitionEithers)

import Control.Concurrent.Async (mapConcurrently)
import Logger.BasicLogger


makePeers :: String
          -> Logger String
          -> TP.ExceptT String IO ([P.Peer], TP.SizeInfo, B.ByteString, [TP.FileInfo])
makePeers tracker log = do
    torrentContent <- BI.parseFromFile tracker
    ipsAndPorts    <- getPeers torrentContent log
    TP.liftEither $ createPeer torrentContent ipsAndPorts log


createPeer :: BI.BEncode
     -> [(N.HostName, N.PortNumber)]
     -> Logger String
     -> Either String
              ([P.Peer], TP.SizeInfo, BC.ByteString, [TP.FileInfo])
createPeer torrentContent ipsAndPorts log = do
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


getPeers :: BI.BEncode
         -> Logger String
         -> TP.ExceptT String IO [(N.HostName, N.PortNumber)]
getPeers torrentContent log = do
    infoH     <- TP.liftEither $ BI.infoHash torrentContent
    announces <- TP.liftEither $ makeAnnounces torrentContent

    ls <- TP.liftIO $ mapConcurrently (\x -> run x torrentContent log) announces
    let (lefts, rights) = partitionEithers ls

    TP.liftIO $ log ("Tracker Errors "++ (show $ length lefts))

    return $ concat rights

makeAnnounces :: BI.BEncode
              -> Either String [BI.AnnounceType]
makeAnnounces torrentContent = do
    announce   <- BI.announce torrentContent
    annouceLst <- BI.announceList torrentContent

    traverse BI.getAnnounce ((annouceLst) )


run :: BI.AnnounceType
    -> BI.BEncode
    -> Logger String
    -> IO (Either String [(N.HostName, N.PortNumber)])
run a b log = TP.runExceptT $ callTracker a b log

callTracker :: BI.AnnounceType
            -> BI.BEncode
            -> Logger String
            -> TP.ExceptT String IO [(N.HostName, N.PortNumber)]
callTracker aType torrentContent log = do
    infoH  <- TP.liftEither $ BI.infoHash torrentContent

    case aType of
        BI.HTTP tracker -> do
            TP.liftIO $ log (show tracker)
            HTTP_T.getHostsAndIps tracker infoH torrentContent

        BI.UDP tracker -> do
            TP.liftIO $ log (show tracker)
            UDP_T.getHostsAndIps tracker infoH
