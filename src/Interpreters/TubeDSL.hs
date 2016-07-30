module Interpreters.TubeDSL
    ( sendHandshake
    , recHandshake
    , decodeMessage
    , recMessage
    , flushLeftOver
    ) where

import Data.Conduit
import qualified Data.Conduit.List as CL

import qualified Peers.Message as M
import qualified Peers.Handshake as H
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Peers.Peer as P
import qualified Data.Sequence as Seq
import qualified Types as TP
import qualified Data.Binary.Get as G
import Control.Monad.Trans.Class (lift)
import Interpreters.Action


data Exec = LastPiece
           | Continue
           | YieldAndContinue


chunkSize = 16384 -- TODO what id last piece is smaler than chunk

sendHandshake :: Monad m => B.ByteString
              -> Sink BC.ByteString m ()
              -> m ()
sendHandshake infoHash peerSink =
   yield handshake $$ peerSink
   where
      handshake = H.createHandshake infoHash


recHandshake ::
   Monad m
   => Sink BC.ByteString m (TP.Perhaps (BC.ByteString, H.Handshake))
recHandshake =
   fmap (maybe (Left "No handshake") convertHandshake) await
   where
      convertHandshake = H.convertParsedOutput . H.decodeHandshake


decodeMessage :: G.Decoder M.Message
              -> Conduit B.ByteString Action M.Message
decodeMessage dec =
    case dec of
        G.Fail {} -> do
            lift $ logF "ERROR: DECODING FAILURE"
            lift $ throwF TP.PacketParseException

        G.Partial fun -> do
            mx <- await
            case mx of
                Nothing -> lift $ throwF TP.NetworkException
                Just x ->  decodeMessage $ fun (Just x)

        (G.Done lo _ x) -> do
            yield x
            leftover lo
            decodeMessage M.getMessage


recMessage :: P.Peer
           -> Conduit M.Message Action (String, BC.ByteString)
recMessage peer = do
  message <- await

  let pieces = P.pieces peer

  case message of
       Nothing -> do
            lift $ logF "EXIT ON NOTHING"
            return ()


       Just (M.Bitfield b) -> do
            let pList = P.bsToPieceLs b
                newPeer = peer {P.pieces = pList}
            lift $ logF "BF"
            lift sendInterestedF

            recMessage newPeer


       Just (M.Have b) -> do
            let p = P.fromBsToInt b
                pList = p : P.pieces peer
                newPeer = peer {P.pieces = pList}

    --        lift $ logF ("Have " ++ (show p))

            recMessage newPeer


       Just M.UnChoke -> do
            lift $  logF "UnChoke"
            nextM <- lift $ requestNextAndUpdateGlobalF pieces
            case nextM of
                 Nothing -> do
                      lift $ logF "EXIT UNCHOKE"
                      return ()

                 Just next -> do
                      lift $ logF ("Req " ++ show next)
                      -- TODO check if its last
                      lift $ sendRequestF (next, 0, chunkSize)
                      lift $ setStatusF next TP.InProgress

                      recMessage peer



       Just (M.Piece p@(idx, offset, chunkBuffer)) -> do
            sizeInfo <- lift getSizeInfoF
            let newBuffer = P.buffer peer `BC.append` chunkBuffer
                newPeer = peer {P.buffer = newBuffer}
                size = getSize idx sizeInfo


            whatToDo <- lift $ handlePiecie sizeInfo (idx,offset) size newPeer

            case whatToDo of
                 LastPiece -> do
                    --        lift $ setStatusF idx TP.Done
                            yield (show idx, newBuffer)
                            lift $ logF ("GOT LAST Piece "
                                        ++ show idx
                                        ++ " "
                                        ++ show offset)
                            return ()

                 Continue -> recMessage newPeer
                 YieldAndContinue -> do   -- Piece Done
                            lift $ setStatusF idx TP.Done
                            yield (show idx, newBuffer)
                            lift $ logF ("GOT Piece "
                                        ++ show idx
                                        ++ " "
                                        ++ show offset)

                            let clearPeer = newPeer {P.buffer = BC.empty}
                            recMessage clearPeer


       Just M.Choke ->
            lift $ throwF TP.ChokeException

       Just M.KeepAlive -> do
            lift $ logF "KeepAlive"
            recMessage peer

       Just y -> do
            lift $ logF ("This message should not arrive while downloading " ++ show y)
            lift $ throwF TP.MsgNotSupportedException


handlePiecie ::
   TP.SizeInfo
   -> (Int, Int)
   -> Int
   -> P.Peer
   -> Action Exec
handlePiecie sizeInfo (idx, offset) pieceSize peer
  | offset < sizeLeft = do

       let newOffset = offset + chunkSize
           reqSize = min sizeLeft chunkSize
       sendRequestF (idx, newOffset , reqSize)
       logF $ show (idx, newOffset , reqSize) --"HashEQ "++ (show hshEq) ++ " "++ (show (BC.length newBuffer))

       return Continue



 | otherwise = do
      let pieces = P.pieces peer
          newBuffer = P.buffer peer
          hshEq = Seq.index (P.pieceHashes peer) idx == P.hashFor newBuffer

      logF $ "HashEQ "
             ++ show hshEq
             ++ " "
             ++ show (BC.length newBuffer)
      setStatusF idx TP.Done


      nextM <- requestNextAndUpdateGlobalF pieces
      case nextM of
            Nothing -> do
                 logF "EXIT"

                 return LastPiece

            Just next -> do
                 logF ("Next " ++ show next)

                 sendRequestF (next, 0 , reqSize next sizeInfo)
                 setStatusF next TP.InProgress

                 return YieldAndContinue

      where
         reqSize next sizeInfo
            | last (P.pieces peer) == next =
                 min (TP.lastPieceSize sizeInfo) chunkSize

            | otherwise =
                 chunkSize

         sizeLeft = pieceSize - chunkSize



getSize next sizeInfo
   | next == (TP.numberOfPieces sizeInfo) - 1 = TP.lastPieceSize sizeInfo
   | otherwise               = TP.normalPieceSize sizeInfo


flushLeftOver :: BC.ByteString
              -> Conduit BC.ByteString Action BC.ByteString
flushLeftOver lo
   | (not . B.null) lo = do
        yield lo
        awaitForever yield

   | otherwise         = awaitForever yield
