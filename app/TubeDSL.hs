-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module TubeDSL where

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Message as M
import Control.Monad.IO.Class
import qualified Handshake as H
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.Binary as CB
import qualified Data.Bits as Bits
import qualified Peer as P
import qualified Control.Concurrent as CC
import qualified Data.Sequence as Seq
import qualified Types as TP
import qualified Data.Array.MArray as MA
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans.Resource
import Data.Maybe
import qualified Data.Binary.Get as G
import Control.Monad.Free (Free(Free,Pure), MonadFree)
import Control.Monad.Trans.Class (lift)
import qualified Data.Conduit.Network as CN





-- =======================

import qualified Control.Monad.Trans as Trans

import qualified Control.Monad.Writer as WT

import Data.Functor.Identity


data Exec = LastPiece
           | Continue
           | YieldAndContinue


          --8192
chunkSize = 16384

sendHandshake :: Monad m => B.ByteString -> Sink BC.ByteString m () -> m ()
sendHandshake infoHash peerSink =
   yield handshake $$ peerSink
   where
      handshake = H.createHandshake infoHash



recHandshake ::
   Monad m
   => Sink BC.ByteString m (TP.Perhaps (BC.ByteString, H.Handshake))
recHandshake =
   await >>= maybe (return $ Left "No handshake")
                   (return . convertHandshake)
   where
      convertHandshake = H.convertParsedOutput . H.decodeHandshake





data HorrentF a = SendInterested a
                | Log String a
                | ReqNextAndUpdate [Int] TP.GlobalPiceInfo ((Maybe Int) -> a)
                | SendRequest (Int, Int, Int) a
                | SetStatus Int TP.GlobalPiceInfo TP.PiceInfo a
                deriving (Functor)


sendInterestedF :: Free HorrentF ()
sendInterestedF = Free $ SendInterested (Pure ())


logF :: String -> Free HorrentF ()
logF str = Free $ Log str (Pure ())


requestNextAndUpdateGlobalF :: [Int]
                            -> TP.GlobalPiceInfo
                            -> Free HorrentF (Maybe Int)
requestNextAndUpdateGlobalF pieces global =
    Free $ ReqNextAndUpdate pieces global Pure

sendRequestF :: (Int, Int, Int) -> Free HorrentF ()
sendRequestF req = Free $ SendRequest req (Pure ())

setStatusF :: Int -> TP.GlobalPiceInfo -> TP.PiceInfo -> Free HorrentF ()
setStatusF x globab status = Free $ SetStatus x globab status (Pure ())


interpret :: Sink BC.ByteString IO () -> Free HorrentF a -> IO a
interpret peerSink program =
    case program of
        Free (SendInterested c) ->
            do  sendInterested peerSink
                interpret peerSink c

        Free (Log str c) ->
            do print ("LOG:: " ++ str)
               interpret peerSink c

        Free (ReqNextAndUpdate pieces global fun) ->
            do m <-requestNextAndUpdateGlobal pieces global
               interpret peerSink (fun m)

        Free (SendRequest req c) ->
            do sendRequest peerSink req
               interpret peerSink c

        Free (SetStatus x global status c) ->
            do setStatus x global status
               interpret peerSink c

        Pure x -> return x



decodeMessage :: G.Decoder M.Message
              -> Conduit BC.ByteString (Free HorrentF) M.Message
decodeMessage dec = do
    case dec of
        (G.Fail _ _ _) ->
            lift $ logF "ERROR: DECODING FAILURE"

        G.Partial fun ->
            await >>= maybe (return ())
                            (\x -> decodeMessage $ fun (Just x))

        (G.Done lo _ x) -> do
            yield x
            leftover lo
            (decodeMessage M.getMessage)


recMessage ::(Monad m) =>
   Sink BC.ByteString m ()
   -> P.Peer
   -> Conduit M.Message (Free HorrentF) (String, BC.ByteString)
recMessage peerSink peer = do
  message <- await

  let pieces = P.pieces peer
      global = P.globalStatus peer

  case message of
       Nothing -> do
            return ()


       Just (M.Bitfield b) -> do
            let pList = P.convertToBits b
                newPeer = peer {P.pieces = pList}
            lift $ logF "BF"
            lift $ sendInterestedF

            recMessage peerSink newPeer


       Just (M.Have b) -> do
            lift $ logF "Have"

            let pList = (P.fromBsToInt b) : P.pieces peer
                newPeer = peer {P.pieces = pList}
            recMessage peerSink newPeer


       Just M.UnChoke -> do
            lift $  logF "UnChoke"
            nextM <- lift $ requestNextAndUpdateGlobalF pieces global
            case nextM of
                 Nothing ->
                      return ()

                 Just next -> do
                      lift $ logF ("Req "++(show next))
                      lift $ sendRequestF (next, 0, chunkSize)
                      lift $ setStatusF next global  TP.InProgress

                      recMessage peerSink peer



       Just (M.Piece (idx, offset, chunkBuffer)) -> do
            let newBuffer = (P.buffer peer) `BC.append` chunkBuffer
                newPeer = peer {P.buffer = newBuffer}
                size = getSize idx (P.sizeInfo peer)


            whatToDo <- handlePiecie peerSink (idx,offset) size newPeer

            case whatToDo of
                 LastPiece -> do
                            yield (show idx, newBuffer)
                            return ()
                 Continue -> recMessage peerSink newPeer
                 YieldAndContinue -> do
                            yield (show idx, newBuffer)
                            let clearPeer = newPeer {P.buffer = BC.empty}
                            recMessage peerSink clearPeer


       Just M.Choke ->
            return ()

       Just M.KeepAlive -> do
            lift $ logF "KeepAlive"
            recMessage peerSink peer

       Just y -> do
            lift $ logF ("This message should not arrive while downloading " ++ (show y))
            return ()





handlePiecie :: (Monad m) =>
   Sink BC.ByteString m ()
   ->(Int, Int)
   -> Int
   -> P.Peer
   -> ConduitM M.Message (String, BC.ByteString) (Free HorrentF) Exec
handlePiecie peerSink (idx, offset) pieceSize peer
  | (offset < sizeLeft) = do

       lift $ logF ((show idx) ++ " " ++ (show offset) ++" "++ (show (BC.length (P.buffer peer))))
       lift $ sendRequestF (idx, offset + chunkSize , min sizeLeft chunkSize)

       return Continue



 | otherwise = do
      let pieces = P.pieces peer
          global = P.globalStatus peer
          newBuffer = P.buffer peer
          hshEq = ((Seq.index (P.peceHashes peer) idx) == P.hashFor newBuffer)

      lift $ logF $ "HashEQ "++ (show hshEq) ++ " "++ (show (BC.length newBuffer))
      lift $ logF $ ""


      nextM <- lift $ requestNextAndUpdateGlobalF pieces global
      case nextM of
            Nothing -> do
                 lift $ logF "EXIT"
                 return LastPiece

            Just next -> do
                 lift $ logF ("Next " ++ (show next))
                 lift $ setStatusF idx (P.globalStatus peer)  TP.Done
                 lift $ sendRequestF (next, 0 , reqSize next)
                 return YieldAndContinue

      where
         reqSize next
            | (last (P.pieces peer) == next) =
                 min (lastS (P.sizeInfo peer)) chunkSize

            | otherwise =
                 chunkSize

         lastS (nbOfPieces, normalSize, lastSize) = lastSize

         sizeLeft = pieceSize - chunkSize






getSize next (nbOfPieces, normalSize, lastSize)
   | (next == nbOfPieces -1) = lastSize
   | otherwise               = normalSize





flushLeftOver :: BC.ByteString
              -> Conduit BC.ByteString (Free HorrentF) BC.ByteString
flushLeftOver lo
   | (not . B.null) lo = do
        yield lo
        awaitForever yield

   | otherwise         = awaitForever yield






tube ::
   P.Peer
   -> ConduitM () BC.ByteString IO () --Source IO BC.ByteString
   -> Sink BC.ByteString IO ()
   -> Sink (String, BC.ByteString) IO ()
   -> IO ()
tube peer getFrom sendTo saveTo = do
   let infoHash = P.infoHash peer
   sendHandshake infoHash sendTo

-- m (ResumableSource m a, b)
   (nextSource, handshake) <- getFrom $$+ recHandshake

   let global     = P.globalStatus peer
       infoSize   = P.sizeInfo peer
       peceHashes = P.peceHashes peer

   case handshake of
      Left l ->
         print $ "Bad Handshake : " ++l

      Right (bitFieldLeftOver, hand) -> do
          let gg = transPipe (interpret sendTo) ((flushLeftOver bitFieldLeftOver)
                =$=  decodeMessage M.getMessage
                =$=  recMessage sendTo peer)
          nextSource $=+ gg $$+- saveTo



--          -> ConduitM M.Message (String, BC.ByteString) (Free HorrentF) Exec

--ll ::  IO a ->  (ConduitM M.Message (String, BC.ByteString) (Free HorrentF) Exec
--(ConduitM i o) IO a
--ll m = lift m




mkSource ::  CN.AppData
         -> ConduitM BC.ByteString BC.ByteString IO ()
mkSource appData = do
    x <- CN.appSource appData
    return x



saveToFile :: Sink (String, BC.ByteString) IO ()
saveToFile =
    awaitForever (lift . save)

save :: (String, BC.ByteString) -> IO()
save (fN, c) =
        runResourceT $
        (yield c)
        $$ (CB.sinkFile ("downloads/" ++ fN))

--transPipe
 -- :: Monad m =>
    -- (forall a. m a -> n a) -> ConduitM i o m r -> ConduitM i o n r

-- lift :: (Monad m, WT.MonadTrans t) => m a -> t m a

--}



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


logMSG :: Conduit BC.ByteString IO BC.ByteString
logMSG = do
--  liftIO $ print "GOT MSG"
  m <- await
  case m of
       Nothing -> return ()
       Just x ->
         do liftIO $ print ("REC " ++ (show (B.length x)))
            yield x
            logMSG
