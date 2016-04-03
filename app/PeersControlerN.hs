
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
import qualified Types as TP
import qualified Data.ByteString as B

import Control.Monad (unless, void)
import qualified TubeDSL as T
import qualified Message as M
import qualified InterpretIO as IPIO

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Streaming.Network as SN

import Control.Concurrent (threadDelay)
import Action
import Control.Monad.Trans.Class (lift)


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



runClient :: TP.GlobalPiceInfo -> TP.SizeInfo -> P.Peer -> IO ()
runClient globalStatus sizeInfo peer =
    CN.runTCPClient (CN.clientSettings (P.port peer) (BC.pack $ P.hostName peer)) $ \appData -> do
        let source =  appSource
            peerSink   = CN.appSink appData -- :: ConduitM B.ByteString () IO ()
        print "TUBE"

        let infoHash = P.infoHash peer
        T.sendHandshake infoHash peerSink

        (IPIO.interpret appData globalStatus sizeInfo peerSink Nothing) (tube peer source)



tube :: P.Peer -> Source Action B.ByteString -> Action ()
tube peer getFrom  = do

   (nextSource, handshake) <- getFrom $$+ T.recHandshake

   case handshake of
      Left l -> logF ("Bad Handshake : " ++l)

      Right (bitFieldLeftOver, hand) -> do
          let gg = ((T.flushLeftOver bitFieldLeftOver)
                =$=  T.decodeMessage M.getMessage
                =$=  T.recMessage peer)

          (nextSource $=+ gg $$+- saveToFile)




appSource :: Producer Action B.ByteString
appSource =
    loop
  where
    read' = readDataWithTimeoutF
    loop = do
        bsM <- lift read'
        case bsM of
            Nothing -> do
                pending <- lift getPendingPieceF
                case pending of
                    Nothing -> return ()
                    Just idx -> do
                        lift $ setStatusF idx (TP.TimeOut "")
                        return ()

            Just bs -> do
                unless (B.null bs) $ do
                    yield bs
                    loop



saveToFile :: Sink ((String, BC.ByteString)) Action ()
saveToFile = do
  awaitForever (lift . save)
  where
    save ::  (String, BC.ByteString) -> Action ()
    save (fN, c) = saveToFileF fN c
