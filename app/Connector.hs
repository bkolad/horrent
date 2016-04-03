{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Connector (makePeers) where

import qualified Peer as P
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
import qualified Types as TP
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Monad.STM
import Data.List

type TorrentContent = BP.BEncode


makePeers :: String -> TP.ExceptT String IO ([P.Peer], TP.SizeInfo)
makePeers tracker =
  do torrentContent <-  BP.parseFromFile tracker
     sizeInfo       <- TP.liftEither $ getSizeInfo torrentContent
     ipsAndPorts    <- peersIpAndPortsFromTracker torrentContent
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
    --             , P.pendingPiece = Nothing
                 }

     let peers = map makePeer ipsAndPorts
     return (peers, sizeInfo)



getSizeInfo :: TorrentContent
            -> Either String TP.SizeInfo
getSizeInfo torrentContent =
  do pieceSize   <- BP.piceSize torrentContent
     torrentSize <- BP.torrentSize torrentContent
     return $ TP.getSizeData torrentSize pieceSize


peersIpAndPortsFromTracker :: TorrentContent
                           -> TP.ExceptT String IO [(N.HostName, N.PortNumber)]
peersIpAndPortsFromTracker torrentContent =
    do urlTracker <- TP.liftEither $ trackerUrl torrentContent
       resp       <- TP.liftIO . getResponseFromTracker $ urlTracker
       peersBS    <- TP.liftEither $ (BP.parseFromBS . BC.pack $ resp) >>= BP.peers
       return $ getIPandPort peersBS


type TrackerResponse = String
type URL = String


getResponseFromTracker :: URL -> IO TrackerResponse
getResponseFromTracker url =
    HTTP.simpleHTTP (HTTP.getRequest url)
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
        get32and16b =
            do empty <- isEmpty
               if empty then
                   return []
               else do
                   ip <- show <$> getWord32be
                   port <- fromIntegral <$> getWord16be
                   rest <- get32and16b
                   return $ (ip, port):rest
