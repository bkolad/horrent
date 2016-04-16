{-# LANGUAGE ScopedTypeVariables #-}
module InterpretIO where

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Message as M
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





import Action



interpret :: SN.AppData
          -> TP.GlobalPiceInfo
          -> TP.SizeInfo
          -> Sink BC.ByteString IO ()
          -> Maybe Int
          -> Action a
          -> IO a
interpret appData global sizeInfo peerSink mPending program =
    case program of
        Free (SendInterested c) -> --liftIO $
            do  sendInterested peerSink

                interpret appData global sizeInfo peerSink mPending c

        Free (Log str c) ->
            do tID <- CC.myThreadId
               print ("LOG:: " ++ str ++" "++ (show tID) )
               interpret appData global sizeInfo peerSink mPending c

        Free (ReqNextAndUpdate pieces fun) ->
            do m <-requestNextAndUpdateGlobal pieces global
               interpret appData global sizeInfo peerSink mPending (fun m)

        Free (SendRequest req c) ->
            do sendRequest peerSink req
               let (pending, _, _) = req
               interpret appData global sizeInfo peerSink (Just pending) c

        Free (SetStatus x status c) ->
            do
               let exception = (TP.PeerException "" (Just x))

               catch (setStatus x global status)
                     (\(e :: SomeException) -> throw exception)


               interpret appData global sizeInfo peerSink mPending c

        Free (ReqSizeInfo fun) ->
            interpret appData global sizeInfo peerSink mPending (fun sizeInfo)

        Free (ReadData fun) ->
            do
               let exception = (TP.PeerException "" mPending)
               m <- catch
                    (TOUT.timeout (2*1000000) (SN.appRead appData))
                    (\(e :: SomeException) -> throw exception)

               case m of
                   Nothing -> throw exception
                   Just d ->
                    interpret appData global sizeInfo peerSink mPending (fun d)

        Free (SaveToFile fN content c) ->
           do R.runResourceT $ (yield content) $$ (CB.sinkFile ("downloads/" ++ fN))
              interpret appData global sizeInfo peerSink mPending c

        Free (GetPendingPiece fun) ->
               interpret appData global sizeInfo peerSink mPending (fun mPending)

        --Free (UnChoke c) ->
        --      interpret appData global sizeInfo peerSink mPending c

        Pure x -> return x



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
                  TP.Registered -> do
                     MA.writeArray global x TP.InProgress
                     return $ Just x
                  TP.TimeOut  -> do
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
