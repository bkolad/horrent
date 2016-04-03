
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module PeersControlerN where

import qualified Connector as CN (makePeers)
import Control.Monad.Except (ExceptT, liftIO, runExceptT)
import qualified Peer as P
import qualified StaticQ as SQ
import qualified Data.Conduit.Network as CN
import Data.Conduit
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit.Binary as CB
import Control.Monad.Trans.Resource
import qualified Types as TP
import qualified Data.ByteString as B

import Control.Monad (unless, void)
import qualified TubeDSL as T
import qualified Message as M
import qualified InterpretIO as IPIO

import qualified System.Timeout as TOUT

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Streaming.Network as SN

import Control.Concurrent (threadDelay)
import Action


main::IO()
main = do result <- runExceptT $ startM "ubuntu.torrent"  -- "tom.torrent"--
          print result


startM :: String -> ExceptT String IO ()
startM tracker =
     do (peers, sizeInfo)  <-  CN.makePeers tracker
        globalStatus       <- liftIO $ TP.newGlobalBitField $ TP.numberOfPieces sizeInfo
        liftIO $ print (length peers)
        liftIO $ threadDelay 1000
        qu                 <- liftIO $ SQ.makeQueueFromList peers-- (take 10 peers)
        liftIO $ SQ.spawnNThreadsAndWait 20 (SQ.loop qu (runClient globalStatus sizeInfo))
        es                 <- liftIO $  (TP.showGlobal globalStatus)
        liftIO $ print (show (filter (\(_, s) -> s /= TP.Done) es))
        return ()


start :: String -> ExceptT String IO ()
start tracker =
     do (peers, sizeInfo)  <-  CN.makePeers tracker
        globalStatus <- liftIO $ TP.newGlobalBitField $ TP.numberOfPieces sizeInfo
        liftIO $ print (length peers)
        let peer = peers !! 1
        liftIO $ print peers
        liftIO $ runClient globalStatus sizeInfo peer
        es                 <- liftIO $  (TP.showGlobal globalStatus)
        liftIO $ print (show es)

        return ()



runClient :: TP.GlobalPiceInfo -> TP.SizeInfo -> P.Peer -> IO ()
runClient globalStatus sizeInfo peer =
    CN.runTCPClient (CN.clientSettings (P.port peer) (BC.pack $ P.hostName peer)) $ \appData -> do
        let source =  appSource appData--CN.appSource appData
            peerSink   = CN.appSink appData
        print "TUBE"

        tube globalStatus sizeInfo peer source peerSink


tube ::
   TP.GlobalPiceInfo
   -> TP.SizeInfo
   -> P.Peer
   -> ConduitM () BC.ByteString IO ()
   -> Sink BC.ByteString IO ()
   -> IO ()
tube global sizeInfo peer getFrom sendTo = do
   let infoHash = P.infoHash peer
   print $ "SNDING HS"
   T.sendHandshake infoHash sendTo
   print $ "SNDING HS DONE"


   (nextSource, handshake) <- getFrom $$+ T.recHandshake


   case handshake of
      Left l ->
         print $ "Bad Handshake : " ++l

      Right (bitFieldLeftOver, hand) -> do
          let gg = transPipe (IPIO.interpret global sizeInfo sendTo) ((T.flushLeftOver bitFieldLeftOver)
                =$=  T.decodeMessage M.getMessage
                =$=  T.recMessage peer)
          nextSource $=+ gg $$+- saveToFile




appSource :: (SN.HasReadWrite ad) => ad -> Producer IO B.ByteString
appSource ad =
    loop
  where
    read' = TOUT.timeout (2 * 1000000) (SN.appRead ad)
    loop = do
        bsM <- liftIO read'
        case bsM of
            Nothing -> do
                liftIO $ print "-----------||||||||||||||"
                return ()
            Just bs -> do
        --       liftIO $ print "-----------||||||||||||||============="
                unless (B.null bs) $ do
                    yield bs
                    loop


saveToFile :: Sink ((String, BC.ByteString)) IO ()
saveToFile = do
  awaitForever (liftIO . save)
  where
    save ::  (String, BC.ByteString) -> IO()
--    save Nothing = return ()
    save ( (fN, c)) =
       runResourceT $
        (yield c)
        $$ (CB.sinkFile ("downloads/" ++ fN))
