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
    infoH     <- TP.liftEither $ BI.infoHash torrentContent
    announces <- TP.liftEither $ makeAnnounces torrentContent

    ls <- TP.liftIO $ mapConcurrently (\x -> (run x) torrentContent) announces
    let (lefts, rights) = partitionEithers ls

    TP.liftIO $ print ("Tracker Errors "++ (show $ length lefts))

    return $ concat rights
    --fmap (concat) $ TP.liftEither $ sequence ls
--    undefined

makeAnnounces :: BI.BEncode -> Either String [BI.AnnounceType]
makeAnnounces torrentContent = do
    announce   <- BI.announce torrentContent
    annouceLst <- BI.announceList torrentContent

    traverse BI.getAnnounce (take 3 annouceLst )


run :: BI.AnnounceType
    -> BI.BEncode
    -> IO (Either String [(N.HostName, N.PortNumber)])
run a b = TP.runExceptT $ callTracker a b

callTracker :: BI.AnnounceType
            -> BI.BEncode
            -> TP.ExceptT String IO [(N.HostName, N.PortNumber)]
callTracker aType torrentContent = do
    infoH  <- TP.liftEither $ BI.infoHash torrentContent

    case aType of
        BI.HTTP tracker -> do
            TP.liftIO $ print tracker
            HTTP_T.getHostsAndIps tracker infoH torrentContent

        BI.UDP tracker -> do
            TP.liftIO $ print tracker
            UDP_T.getHostsAndIps tracker infoH
