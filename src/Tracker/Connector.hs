{-# LANGUAGE NoMonomorphismRestriction, ConstraintKinds, ScopedTypeVariables, FlexibleContexts #-}

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



tryEither e = case e of
    Left l -> throwError l
    Right x -> return x

makePeers :: ( MonadLogger m
             , MonadIO m
             , MonadError String m)
          => String
          ->  m ([P.Peer], TP.SizeInfo, B.ByteString, [TP.FileInfo])
makePeers tracker = do
    logMessage $ T.pack ("Start1 ")
    torrentContent <-  liftIO $ runExceptT $ BI.parseFromFile tracker
    logMessage $ T.pack ("Start2 ")

    tc <- tryEither torrentContent
    logMessage $ T.pack ("Start3 ")

    ipsAndPorts <- getPeers tc
    logMessage $ T.pack (("Start4 ") ++ (show ipsAndPorts))

    tryEither $ createPeer tc ipsAndPorts


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

getPeers :: ( MonadLogger m
            , MonadIO m
            , MonadError String m)
         => BI.BEncode
         -> m [(N.HostName, N.PortNumber)]
getPeers torrentContent = do
    let (Right infoH)   = BI.infoHash torrentContent
        Right announces = makeAnnounces torrentContent

    logMessage $ T.pack ("GP1 ")

    ls <- TP.liftIO $ mapConcurrently (\x -> run x torrentContent ) announces
    let (lefts, rights) = partitionEithers ls

    logMessage $ T.pack ("Tracker Errors "++ (show $ length lefts) ++ (show lefts))

    return $ concat rights



makeAnnounces :: BI.BEncode
              -> Either String [BI.AnnounceType]
makeAnnounces torrentContent = do
    announce   <- BI.announce torrentContent
    annouceLst <- BI.announceList torrentContent

    traverse BI.getAnnounce ((announce : annouceLst) )


run :: BI.AnnounceType
    -> BI.BEncode
    -> IO (Either String [(N.HostName, N.PortNumber)])
run a b  = TP.runExceptT $ callTracker a b

callTracker :: BI.AnnounceType
            -> BI.BEncode
            -> TP.ExceptT String IO [(N.HostName, N.PortNumber)]
callTracker aType torrentContent  = do
    infoH  <- TP.liftEither $ BI.infoHash torrentContent

    case aType of
        BI.HTTP tracker -> do
    --        TP.liftIO $ print (show tracker)
            HTTP_T.getHostsAndIps tracker infoH torrentContent

        BI.UDP tracker -> do
        --    TP.liftIO $ log (show tracker)
            UDP_T.getHostsAndIps tracker infoH
