module InterpretIO where

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Message as M
import qualified Data.ByteString.Char8 as BC
import qualified Types as TP
import qualified Data.Array.MArray as MA
import qualified Control.Concurrent.STM as STM

import Control.Monad.Trans.Class (lift)

import Action

interpret :: TP.GlobalPiceInfo -> Sink BC.ByteString IO () -> Action a -> IO a
interpret global peerSink program =
    case program of
        Free (SendInterested c) ->
            do  sendInterested peerSink
                interpret global peerSink c

        Free (Log str c) ->
            do print ("LOG:: " ++ str)
               interpret global peerSink c

        Free (ReqNextAndUpdate pieces fun) ->
            do m <-requestNextAndUpdateGlobal pieces global
               interpret global peerSink (fun m)

        Free (SendRequest req c) ->
            do sendRequest peerSink req
               interpret global peerSink c

        Free (SetStatus x status c) ->
            do setStatus x global status
               interpret global peerSink c

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
