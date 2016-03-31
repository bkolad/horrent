-- {-# LANGUAGE FlexibleInstances #-}

module TubeDSL ( sendHandshake
               , recHandshake
               , decodeMessage
               , recMessage
               , flushLeftOver
               ) where

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
import Action


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
            lift $ logF "DECODE DIONE"

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
            sizeInfo <- lift $ getSizeInfoF
            let newBuffer = (P.buffer peer) `BC.append` chunkBuffer
                newPeer = peer {P.buffer = newBuffer}
                size = getSize idx sizeInfo

            lift $ logF ("GOT Piece " ++ (show idx) ++" "++ (show offset))

            lift $ logF ("Size Info " ++ (show sizeInfo) ++" ")

            whatToDo <- handlePiecie sizeInfo (idx,offset) size newPeer

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
   TP.SizeInfo
   -> (Int, Int)
   -> Int
   -> P.Peer
   -> ConduitM M.Message (String, BC.ByteString) Action Exec
handlePiecie sizeInfo (idx, offset) pieceSize peer
  | (offset < sizeLeft) = do

       let newOffset = offset + chunkSize
           reqSize = min sizeLeft chunkSize
       lift $ logF ((show sizeInfo)++"ReqN "++ (show idx) ++ " " ++ (show newOffset) ++" "++" " ++(show reqSize)++" "++(show (BC.length (P.buffer peer))))
       lift $ sendRequestF (idx, newOffset , reqSize)

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
                 lift $ setStatusF idx TP.Done

                 lift $ sendRequestF (next, 0 , reqSize next sizeInfo)
                 return YieldAndContinue

      where
         reqSize next sizeInfo
            | (last (P.pieces peer) == next) =
                 min (TP.lastPieceSize sizeInfo) chunkSize

            | otherwise =
                 chunkSize

         sizeLeft = pieceSize - chunkSize



getSize next sizeInfo
   | (next == (TP.numberOfPieces sizeInfo) - 1) = TP.lastPieceSize sizeInfo
   | otherwise               = TP.normalPieceSize sizeInfo


flushLeftOver :: BC.ByteString
              -> Conduit BC.ByteString Action BC.ByteString
flushLeftOver lo
   | (not . B.null) lo = do
        yield lo
        awaitForever yield

   | otherwise         = awaitForever yield
