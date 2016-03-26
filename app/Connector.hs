{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, DoAndIfThenElse #-}

module Connector ( liftEither, makePeers) where

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
import Types as TP
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Monad.STM
import Data.List

type TorrentContent = BP.BEncode



logMsg a = liftIO $ print a



makePeers :: String -> ExceptT String IO [P.Peer]
makePeers tracker =
  do torrentContent              <-  BP.parseFromFile tracker
     info@(numberOfPieces, _, _) <- liftEither $ getSizeInfo torrentContent
     liftIO $ print (show info)
     globalStatus                <- liftIO $ TP.newGlobalBitField numberOfPieces
     liftIO $ print ("GET IPS")
     ipsAndPorts                 <- peersIpAndPortsFromTracker torrentContent

     infoHash                    <- liftEither $ BC.pack <$> BP.infoHash torrentContent
     pHashes                     <- liftEither $ BP.piecesHashSeq torrentContent
     let peers = map (\(host, p) -> P.Peer host (fromIntegral p) [] infoHash globalStatus False BC.empty pHashes info) ipsAndPorts
     liftIO $ print ("GOT IPS")

     return peers




{--
getInfoHash :: String -> ExceptT String IO B.ByteString
getInfoHash tracker =
     do torrentContent <-  BP.parseFromFile tracker
        infoHash <- liftEither $ BC.pack <$> BP.infoHash torrentContent
        return infoHash

--}

getSizeInfo ::
    TorrentContent ->
    Either String (TP.NumberOfPieces, TP.NormalPieceSize, TP.LastPieceSize)
getSizeInfo torrentContent =
  do pieceSize   <- BP.piceSize torrentContent
     torrentSize <- BP.torrentSize torrentContent
     return $ getSizeData torrentSize pieceSize
       where
         getSizeData torrentSize pieceSize =
           let tSize = fromIntegral torrentSize
               pSize = fromIntegral pieceSize
               numberOfPieces = ceiling $ tSize / pSize
               lastPieceSize = tSize `mod` pSize
           in (numberOfPieces, pSize, lastPieceSize)



peersIpAndPortsFromTracker ::
    TorrentContent
    -> ExceptT String IO [(N.HostName, N.PortNumber)]
peersIpAndPortsFromTracker torrentContent =
    do urlTracker <- liftEither $ trackerUrl torrentContent
       resp       <- liftIO . getResponseFromTracker $ urlTracker
       peersBS    <- liftEither $ (BP.parseFromBS . BC.pack $ resp) >>= BP.peers
       return $ getIPandPort peersBS



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
