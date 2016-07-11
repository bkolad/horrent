{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Interpreters.IO ( InterpreterEnv (..)
                   , interpret
                   ) where

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Peers.Message as M
import qualified Data.ByteString.Char8 as BC
import qualified Types as TP
import qualified Data.Array.MArray as MA
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent as CC
import Control.Monad.Trans.Class (lift)
--import Control.Monad.IO.Class
import qualified System.Timeout as TOUT
import qualified Data.Streaming.Network as SN
import qualified Data.Conduit.Binary as CB
import qualified Control.Monad.Trans.Resource as R
import Control.Exception


import Control.Monad.IO.Class
import Control.Monad.Reader


import Interpreters.Action

data InterpreterEnv =
    InterpreterEnv { appData  :: SN.AppData
                   , global   :: TP.GlobalPiceInfo
                   , sizeInfo :: TP.SizeInfo
                   , peerSink :: Sink BC.ByteString IO ()
                   , pending  :: Maybe Int
                   , host     :: String
                   }

interpret :: Action a
          -> ReaderT InterpreterEnv IO a
interpret program =
    case program of
        Free (SendInterested c) ->
            do pSink <- peerSink <$> ask
               liftIO $ sendInterested pSink
               interpret c

        Free (Log str c) ->
            do tID <- liftIO CC.myThreadId
               liftIO $ print ("LOG:: " ++ str ++" "++ (show tID) )
               interpret c

        Free (ReqNextAndUpdate pieces fun) ->
            do gl <- global <$> ask
               next <- liftIO $ requestNextAndUpdateGlobal pieces gl
               interpret $ fun next

        Free (SendRequest req c) ->
            do pSink <- peerSink <$> ask
               hostName <- host <$> ask
               let (pend, _, _) = req
                   exception =
                        makeException TP.NetworkException hostName (Just pend)

               liftIO $ catch (sendRequest pSink req)
                              (\(e :: SomeException) -> throw exception)

               local ( \ env -> env {pending = (Just pend)}) (interpret c)


        Free (SetStatus x status c) ->
            do gl <- global <$> ask
               liftIO $ (setStatus x gl status)
               interpret c

        Free (ReqSizeInfo fun) ->
            do sInfo <- sizeInfo <$> ask
               interpret $ fun sInfo


        Free (ReadData t fun) ->
            do
               mPending <- pending <$> ask
               aData <- appData <$> ask
               hostName <- host <$> ask

               let networkExcpetion =
                       makeException TP.NetworkException hostName mPending

               mTOut <- liftIO $ catch
                            (TOUT.timeout t (SN.appRead aData))
                            (\(e :: SomeException) -> throw networkExcpetion)

               let timeOutException =
                       makeException TP.TimeOutException hostName mPending

               maybe (throw timeOutException)
                     (\d -> interpret (fun d))
                     mTOut


        Free (SaveToFile fN content c) ->
           do liftIO $ R.runResourceT $ (yield content)
                                      $$ (CB.sinkFile fN)
              interpret c

        Free (GetPendingPiece fun) ->
            do mPending <- pending <$> ask
               interpret (fun mPending)

        Free (Throw ex c) ->
            do mPending <- pending <$> ask
               hostName <- host <$> ask
               let exception =
                      makeException ex hostName mPending

               liftIO $ throw exception


        Pure x -> return x


makeException reason hName pending =
    TP.PeerException reason hName pending


requestNextAndUpdateGlobal :: [Int] -> TP.GlobalPiceInfo -> IO (Maybe Int)
requestNextAndUpdateGlobal pics global =
   STM.atomically $ reqNext pics global
      where
         reqNext :: [Int] -> TP.GlobalPiceInfo -> STM.STM (Maybe Int)
         reqNext [] _ = return Nothing
         reqNext (x:xs) global =
            do pInfo <- MA.readArray global x -- TODO view pattern
               case pInfo of
                  TP.NotHave -> do
                     MA.writeArray global x TP.InProgress
                     return $ Just x
                  TP.InProgress -> reqNext xs global
                  TP.Done -> reqNext xs global


setStatusDone :: Int -> TP.GlobalPiceInfo -> IO()
setStatusDone x global =
    STM.atomically $ MA.writeArray global x TP.Done


setStatus :: Int -> TP.GlobalPiceInfo -> TP.PiceInfo -> IO()
setStatus x global status =
    STM.atomically $ MA.writeArray global x status


printArray :: TP.GlobalPiceInfo -> IO()
printArray global = do
    k <- STM.atomically $ MA.getElems global
    print $ zip k [0..]


sendInterested :: Sink BC.ByteString IO () -> IO()
sendInterested peerSink =
    yield (M.encodeMessage M.Interested)
    $$ peerSink


sendRequest :: Sink BC.ByteString IO () -> (Int, Int, Int) -> IO()
sendRequest peerSink req =
    yield (M.encodeMessage $ M.Request req)
    $$ peerSink
