
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
import qualified Control.Monad as M
import Control.Exception
import qualified Data.Array.MArray as MA
import qualified Control.Concurrent.STM as STM




main::IO()
main = do result <- runExceptT $ startM "ub222.torrent" --"ubuntu.torrent"  -- "tom.torrent"--
          print result


startM :: String -> ExceptT String IO ()
startM tracker =
     do (peers, sizeInfo)  <-  CN.makePeers tracker
        globalStatus       <- liftIO $ TP.newGlobalBitField $ TP.numberOfPieces sizeInfo
        liftIO $ print (length peers)
        liftIO $ threadDelay 1000
        qu                 <- liftIO $ SQ.makeQueueFromList peers-- (take 10 peers)
        lStat <- liftIO $ SQ.spawnNThreadsAndWait 20 (SQ.loop qu (runClientSafeSSS globalStatus sizeInfo) [])
        liftIO $ print (show (filter (\s -> s /= OK) $ M.join lStat))

        es                 <- liftIO $  (TP.showGlobal globalStatus)
        liftIO $ print (show (filter (\(_, s) -> s /= TP.Done) es))
        return ()


start :: [P.Peer] -> TP.SizeInfo -> [PeerStatus] -> IO [PeerStatus]
start peers sizeInfo ps = do

    globalStatus <- liftIO $ TP.newGlobalBitField $ TP.numberOfPieces sizeInfo
    qu           <- liftIO $ SQ.makeQueueFromList peers-- (take 10 peers)
    lStat <- liftIO $ SQ.spawnNThreadsAndWait 20 (SQ.loop qu (runClientSafe globalStatus sizeInfo) [])
    return $ M.join lStat


makeGlobal :: [PeerStatus] -> TP.SizeInfo-> IO TP.GlobalPiceInfo
makeGlobal ps sizeInfo =
    do globalStatus <- liftIO $ TP.newGlobalBitField $ TP.numberOfPieces sizeInfo
     --  mapM_ ()
       return globalStatus


changeStatus :: PeerStatus -> TP.GlobalPiceInfo -> IO ()
changeStatus OK _ = return ()
changeStatus (Error n i) global = return ()




data PeerStatus = OK | Error String Int
    deriving (Show, Eq)

runClientSafeSSS ::  TP.GlobalPiceInfo -> TP.SizeInfo -> P.Peer -> IO PeerStatus
runClientSafeSSS globalStatus sizeInfo peer = do
    catch (runClientSafe globalStatus sizeInfo peer)
          (\(e :: SomeException) -> do print "!!!!!!!!!!!!!!!!!!!!!!"
                                       return $ Error (P.hostName peer) (-999))


runClientSafe ::  TP.GlobalPiceInfo -> TP.SizeInfo -> P.Peer -> IO PeerStatus
runClientSafe globalStatus sizeInfo peer = do
    let rr = (\ _ -> OK) <$> (runClient globalStatus sizeInfo peer)
    catch rr
          (\(TP.PeerException n iM) ->
          case iM of
             Nothing -> return $ Error (P.hostName peer) 99999
             Just i -> do setStatusTimetOut i globalStatus
                          return $ Error (P.hostName peer) i)



runClient :: TP.GlobalPiceInfo -> TP.SizeInfo -> P.Peer -> IO ()
runClient globalStatus sizeInfo peer =
    CN.runTCPClient (CN.clientSettings (P.port peer) (BC.pack $ P.hostName peer)) $ \appData -> do
        print "Start"
        let source =  appSource
            peerSink   = CN.appSink appData
        print "TUBE"

        let infoHash = P.infoHash peer
        T.sendHandshake infoHash peerSink

        let action = (tube peer source)
            run = (IPIO.interpret appData globalStatus sizeInfo peerSink Nothing)

        run action


setStatusTimetOut :: Int -> TP.GlobalPiceInfo -> IO()
setStatusTimetOut x global =
    STM.atomically $ MA.writeArray global x TP.TimeOut


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
        bs <- lift read'
        unless (B.null bs) $ do
            yield bs
            loop



saveToFile :: Sink ((String, BC.ByteString)) Action ()
saveToFile = do
  awaitForever (lift . save)
  where
    save ::  (String, BC.ByteString) -> Action ()
    save (fN, c) = saveToFileF fN c
