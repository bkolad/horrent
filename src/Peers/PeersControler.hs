
module Peers.PeersControler (start) where

import qualified Tracker.Connector as CN (makePeers)
import qualified Peers.Peer as P
import qualified StaticQ as SQ
import qualified Data.Conduit.Network as CN
import Data.Conduit
import qualified Data.ByteString.Char8 as BC
import qualified Types as TP
import qualified Data.ByteString as B
import qualified Interpreters.TubeDSL as T
import qualified Peers.Message as M
import qualified Interpreters.IO as IPIO
import Control.Monad.IO.Class (MonadIO (liftIO))
import Interpreters.Action
import Control.Monad (join)
import Control.Exception
import qualified Data.Array.MArray as MA
import qualified Control.Concurrent.STM as STM

import  Control.Monad.Reader (runReaderT, lift, unless)


start tracker =
     do (peers, sizeInfo, fN)  <-  CN.makePeers tracker
        liftIO $ print $ length peers
        liftIO $ print sizeInfo

        globalStatus       <- liftIO $ makeGlobal sizeInfo
        qu                 <- liftIO $ SQ.makeQueueFromList peers
        liftIO $ print "START"
        lStat              <- liftIO $ download 100 qu globalStatus sizeInfo
        let problems = filter (OK /=) $ join lStat
        missing <- liftIO $ missingPieces globalStatus
        return (problems, missing, show fN)

        where
            makeGlobal sizeInfo =
                TP.newGlobalBitField $ TP.numberOfPieces sizeInfo

            download pN qu globalStatus sizeInfo = do
                let run = SQ.loop qu (runClientSafe globalStatus sizeInfo) []
                SQ.spawnNThreadsAndWait pN run

            missingPieces globalStatus = do
                es <- TP.showGlobal globalStatus
                return $ filter (\(_, s) -> s /= TP.Done) es



data PeerStatus = OK
                | TubeError String (Maybe Int)
                | HandShakeError String
    deriving (Show, Eq)


runClientSafe ::  TP.GlobalPiceInfo -> TP.SizeInfo -> P.Peer -> IO PeerStatus
runClientSafe globalStatus sizeInfo peer =
    catches (runClient globalStatus sizeInfo peer)
          [ tubeExceptionHandler globalStatus
          , ioExceptionHandler peer ]


tubeExceptionHandler globalStatus =
    Handler handle
    where
        handle (TP.PeerException e host iM) =
            case iM of
                Nothing ->
                    return (TubeError host iM)
                Just x -> do
                    setStatusNotHave x globalStatus
                    return (TubeError host iM)


ioExceptionHandler peer =
    Handler (\ (SomeException ex) ->
        return $ HandShakeError (P.hostName peer++" "++show ex))


setStatusNotHave :: Int -> TP.GlobalPiceInfo -> IO()
setStatusNotHave x global =
    STM.atomically $ MA.writeArray global x TP.NotHave


runClient :: TP.GlobalPiceInfo -> TP.SizeInfo -> P.Peer -> IO PeerStatus
runClient globalStatus sizeInfo peer = CN.runTCPClient
    (CN.clientSettings (P.port peer) (BC.pack $ P.hostName peer))
        $ \appData -> do

            let source =  appSource
                peerSink   = CN.appSink appData

            let infoHash = P.infoHash peer
            T.sendHandshake infoHash peerSink

            let action = tube peer source
                env = IPIO.InterpreterEnv { IPIO.appData  = appData
                                          , IPIO.global   = globalStatus
                                          , IPIO.sizeInfo = sizeInfo
                                          , IPIO.peerSink = peerSink
                                          , IPIO.pending  = Nothing
                                          , IPIO.host     = P.hostName peer
                                          }

            runReaderT (IPIO.interpret action) env


setStatusTimetOut :: Int -> TP.GlobalPiceInfo -> IO()
setStatusTimetOut x global =
    STM.atomically $ MA.writeArray global x TP.NotHave


tube :: P.Peer -> Source Action B.ByteString -> Action PeerStatus
tube peer getFrom  = do

   (nextSource, handshake) <- getFrom $$+ T.recHandshake

   case handshake of
      Left l -> do
          let msg = "Bad Handshake : " ++l
          logF msg
          return $ HandShakeError msg

      Right (bitFieldLeftOver, hand) -> do
          let gg = T.flushLeftOver bitFieldLeftOver
                =$=  T.decodeMessage M.getMessage
                =$=  T.recMessage peer

          nextSource $=+ gg $$+- saveToFile



timeOut = 100000000

appSource :: Source Action B.ByteString
appSource =
    loop
  where
    read' = readDataWithTimeoutF timeOut
    loop = do
        bs <- lift read'
        unless (B.null bs) $ do
            yield bs
            loop


saveToFile :: Sink (String, BC.ByteString) Action PeerStatus
saveToFile = do
    mX <- await
    case mX of
        Nothing ->
            return OK
        Just (fN, c) -> do
            lift $ saveToFileF ("downloads/" ++fN) c
            saveToFile
