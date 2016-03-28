-- {-# LANGUAGE FlexibleInstances #-}

module TubeDSL where

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Message as M
import qualified Handshake as H
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Peer as P
import qualified Data.Sequence as Seq
import qualified Types as TP
import qualified Data.Binary.Get as G
import Control.Monad.Trans.Class (lift)
import qualified Data.Conduit.Network as CN
import InterpretIO
import Action

import Debug.Trace


data Exec = LastPiece
           | Continue
           | YieldAndContinue


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


decodeMessage :: G.Decoder M.Message
              -> Conduit BC.ByteString Action M.Message
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


recMessage ::
   P.Peer
   -> Conduit M.Message Action (String, BC.ByteString)
recMessage peer = do
  message <- await

  let pieces = P.pieces peer

  case message of
       Nothing -> do
            return ()


       Just (M.Bitfield b) -> do
            let pList = P.bsToPieceLs b
                newPeer = peer {P.pieces = pList}
            lift $ logF "BF"
            lift $ sendInterestedF

            recMessage newPeer


       Just (M.Have b) -> do
            let p = (P.fromBsToInt b)
                pList = p : P.pieces peer
                newPeer = peer {P.pieces = pList}

            lift $ logF ("Have " ++ (show p))

            recMessage newPeer


       Just M.UnChoke -> do
            lift $  logF "UnChoke"
            nextM <- lift $ requestNextAndUpdateGlobalF pieces
            case nextM of
                 Nothing ->
                      return ()

                 Just next -> do
                      lift $ logF ("Req "++(show next))
                      lift $ sendRequestF (next, 0, chunkSize)
                      lift $ setStatusF next TP.InProgress
                      lift $ logF ("Req NN")

                      recMessage peer



       Just (M.Piece p@(idx, offset, chunkBuffer)) -> do
            let newBuffer = (P.buffer peer) `BC.append` chunkBuffer
                newPeer = peer {P.buffer = newBuffer}
                size = getSize idx (P.sizeInfo peer)

            let (idxa, offseta) = trace ("REC "++ (show (idx, offset))) (idx, offset)

            lift $ logF ("GOT Piece " ++ (show idx) ++" "++ (show offset))

            whatToDo <- handlePiecie (idxa,offseta) size newPeer

            case whatToDo of
                 LastPiece -> do
                            yield (show idx, newBuffer)
                            return ()
                 Continue -> recMessage newPeer
                 YieldAndContinue -> do   -- Piece Done
                            yield (show idx, newBuffer)
                            let clearPeer = newPeer {P.buffer = BC.empty}
                            recMessage clearPeer


       Just M.Choke ->
            return ()

       Just M.KeepAlive -> do
            lift $ logF "KeepAlive"
            recMessage peer

       Just y -> do
            lift $ logF ("This message should not arrive while downloading " ++ (show y))
            return ()





handlePiecie ::
   (Int, Int)
   -> Int
   -> P.Peer
   -> ConduitM M.Message (String, BC.ByteString) Action Exec
handlePiecie (idx, offset) pieceSize peer
  | (offset < sizeLeft) = do

       let newOffset = offset + chunkSize
       let (iii, no) = trace ("ASK FOR " ++ (show(idx, newOffset))) (idx, newOffset)

       --lift $ logF ("ReqN "++ (show idx) ++ " " ++ (show offset) ++" "++ (show (BC.length (P.buffer peer))))
       lift $ sendRequestF (iii, no , min sizeLeft chunkSize)

       return Continue



 | otherwise = do
      let pieces = P.pieces peer
          newBuffer = P.buffer peer
    --      hshEq = ((Seq.index (P.pieceHashes peer) idx) == P.hashFor newBuffer)



      --lift $ logF $ "HashEQ "++ (show hshEq) ++ " "++ (show (BC.length newBuffer))
      lift $ logF $ ""


      nextM <- lift $ requestNextAndUpdateGlobalF pieces
      case nextM of
            Nothing -> do
                 lift $ logF "EXIT"
                 return LastPiece

            Just next -> do
                 lift $ logF ("Next " ++ (show next))
                 let idxA = trace ("NEXT P "++ (show next)) next
                 lift $ setStatusF idx TP.Done
                 lift $ sendRequestF (idxA, 0 , reqSize next)
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
              -> Conduit BC.ByteString Action BC.ByteString
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
   print $ "SNDING HS"
   sendHandshake infoHash sendTo
   print $ "SNDING HS DONE"


   (nextSource, handshake) <- getFrom $$+ recHandshake

   let global     = P.globalStatus peer
       infoSize   = P.sizeInfo peer

   case handshake of
      Left l ->
         print $ "Bad Handshake : " ++l

      Right (bitFieldLeftOver, hand) -> do
          let gg = transPipe (interpret global sendTo) ((flushLeftOver bitFieldLeftOver)
                =$=  decodeMessage M.getMessage
                =$=  recMessage peer)
          nextSource $=+ gg $$+- saveTo
