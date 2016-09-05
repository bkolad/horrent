
module Peers.PeersControler (start) where

import qualified Peers.Peer as P
import qualified StaticQ as SQ
import qualified Data.Conduit.Network as C
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
import Logger.BasicLogger --(MonadLogger)


import  Control.Monad.Reader (runReaderT, lift, unless)

start :: ( MonadIO m
         , MonadLogger m l)
      => [P.Peer]
      -> TP.SizeInfo
      -> String
      -> m ([PeerStatus], [(Int, TP.PiceInfo)])
start peers sizeInfo downloadsDir = do

    globalStatus <- liftIO $ makeGlobal sizeInfo
    qu           <- liftIO $ SQ.makeQueueFromList peers
    lStat        <- liftIO $ download downloadsDir 500 qu globalStatus sizeInfo
    let problems = filter (OK /=) $ join lStat
    missing      <- liftIO $ missingPieces globalStatus
    return (problems, missing)

    where
        makeGlobal sizeInfo =
            TP.newGlobalBitField $ TP.numberOfPieces sizeInfo

        download dir pN qu globalStatus sizeInfo = do
            let run = SQ.loop qu (runClientSafe dir globalStatus sizeInfo) []
            SQ.spawnNThreadsAndWait pN run

        missingPieces globalStatus = do
            es <- TP.showGlobal globalStatus
            return $ filter (\(_, s) -> s /= TP.Done) es



data PeerStatus = OK
                | TubeError String (Maybe Int)
                | HandShakeError String
    deriving (Show, Eq)


runClientSafe :: String
              -> TP.GlobalPiceInfo
              -> TP.SizeInfo
              -> P.Peer
              -> IO PeerStatus
runClientSafe dir globalStatus sizeInfo peer =
    catches (runClient dir globalStatus sizeInfo peer)
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


setStatusNotHave :: Int
                 -> TP.GlobalPiceInfo
                 -> IO()
setStatusNotHave x global =
    STM.atomically $ MA.writeArray global x TP.NotHave


runClient :: String
          -> TP.GlobalPiceInfo
          -> TP.SizeInfo
          -> P.Peer
          -> IO PeerStatus
runClient dir globalStatus sizeInfo peer = C.runTCPClient
    (C.clientSettings (P.port peer) (BC.pack $ P.hostName peer))
        $ \appData -> do

            let source =  appSource
                peerSink   = C.appSink appData

            let infoHash = P.infoHash peer
            T.sendHandshake infoHash peerSink

            let action = tube dir peer source
                env = IPIO.InterpreterEnv { IPIO.appData  = appData
                                          , IPIO.global   = globalStatus
                                          , IPIO.sizeInfo = sizeInfo
                                          , IPIO.peerSink = peerSink
                                          , IPIO.pending  = Nothing
                                          , IPIO.host     = P.hostName peer
                                          }

            runReaderT (IPIO.interpret action) env


setStatusTimetOut :: Int
                  -> TP.GlobalPiceInfo
                  -> IO()
setStatusTimetOut x global =
    STM.atomically $ MA.writeArray global x TP.NotHave


tube :: String
      -> P.Peer
      -> Source Action B.ByteString
      -> Action PeerStatus
tube dir peer getFrom  = do

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

          nextSource $=+ gg $$+- saveToFile dir



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


saveToFile :: String
           -> Sink (String, BC.ByteString) Action PeerStatus
saveToFile dir = do
    mX <- await
    case mX of
        Nothing ->
            return OK
        Just (fN, c) -> do
            lift $ saveToFileF (dir ++ "/" ++ fN ++ ".part") c
            saveToFile dir
