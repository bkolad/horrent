{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, FlexibleContexts,FlexibleInstances,UndecidableInstances, Rank2Types #-}

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
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Except
import qualified Data.Text as T
import Horrent



makePeers :: MonadHorrent m l String
          => String
          ->  m ([P.Peer], TP.SizeInfo, B.ByteString, [TP.FileInfo])
makePeers tracker = do
    torrentContent <- BI.parseFromFile tracker
    ipsAndPorts    <- getPeers torrentContent
    TP.tryEither $ createPeer torrentContent ipsAndPorts


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


type UMonadIO = MonadIO

getPeers :: MonadHorrent m l String
         => BI.BEncode
         -> m [(N.HostName, N.PortNumber)]
getPeers torrentContent = do
    let (Right infoH)   = BI.infoHash torrentContent
        Right announces = makeAnnounces torrentContent

    logMessageS ("GP1 ")

    ls <- mapConcUnliftIO (callTracker torrentContent) announces
    let (lefts, rights) = partitionEithers ls

    logMessageS ("Tracker Errors "++ (show $ length lefts) ++ (show lefts))
--    logMessageS $ show rights

    return $ concat rights


mapConcUnliftIO :: MonadHorrent m l String
                    => (forall m1. (MonadHorrent m1 l String)
                            => a
                            -> m1 [b])
                    -> [a]
                    -> m [Either String [b]]
mapConcUnliftIO action ls = do
    h <- getHandle
    let run x = runExceptT $ (runLogger (action x) h)
    TP.liftIO $ mapConcurrently run ls


makeAnnounces :: BI.BEncode
              -> Either String [BI.AnnounceType]
makeAnnounces torrentContent = do
    announce   <- BI.announce torrentContent
    annouceLst <- BI.announceList torrentContent

    traverse BI.getAnnounce (([announce]) )

callTracker :: MonadHorrent m l String
            => BI.BEncode
            -> BI.AnnounceType
            -> m [(N.HostName, N.PortNumber)]
callTracker torrentContent aType  = do
    infoH  <- TP.tryEither $ BI.infoHash torrentContent

    case aType of
        BI.HTTP tracker -> do
            logMessageS (show tracker)
            HTTP_T.getHostsAndIps tracker infoH torrentContent

        BI.UDP tracker -> do
        --    TP.liftIO $ log (show tracker)
            UDP_T.getHostsAndIps tracker infoH
--}



{--
run :: Logable l =>
    l
    -> BI.AnnounceType
    -> BI.BEncode
    -> IO (Either String [(N.HostName, N.PortNumber)])
run h a b  = runExceptT $ runLogger ( callTracker a b) h
--}

bar :: MonadIO m
    => m String
bar = undefined

run2IO :: MonadIO m
       => (m String)
       -> m String
run2IO foo  = liftIO bar

run3IO :: MonadIO m => (forall m1. MonadIO m1 => m1 String) -> m String
run3IO foo = liftIO foo


--kk = run2IO bar
