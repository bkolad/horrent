module Simulator where

import qualified Tube as T
import qualified Peer as P
import qualified Data.ByteString.Char8 as BC
import qualified Handshake as H
import qualified Message as M
import qualified Types as TP

import Control.Monad.IO.Class
import Data.Conduit




hash = "01234567890123456789"


chunkSize = 16384
pieceSize = 2*chunkSize

bs = BC.replicate chunkSize '1'

bsLast = BC.replicate 11 '1'


hashP = P.hashFor (BC.append bs bs)


peer :: IO P.Peer
peer = do
  global <- (TP.newGlobalBitField 3)
  return $ P.Peer "Host!" 0 [] (BC.pack  hash) global True (BC.empty) (TP.hashInfoFromList [hashP, hashP, P.hashFor bsLast])  (3, pieceSize, 16385)


  
source :: Source IO BC.ByteString
source = do yield $ H.createHandshake (BC.pack hash) 
            yield $ M.encodeMessage $ M.Bitfield (P.hasIdxs [0,1,2])
            yield $ M.encodeMessage $ M.UnChoke
            yield $ M.encodeMessage $ M.Piece(0, 0, bs)
            yield $ M.encodeMessage $ M.Piece(0, chunkSize, bs)
        
            yield $ M.encodeMessage $ M.Piece(1, 0, bs)
            yield $ M.encodeMessage $ M.Piece(1, chunkSize, bs)
            
            yield $ M.encodeMessage $ M.Piece(2, 0, bsLast)
            yield $ M.encodeMessage $ M.Piece(2, chunkSize, bsLast)
          
            
            
                           
sink :: Sink BC.ByteString IO ()
sink = do xm <- await
          case xm of 
               Nothing -> return ()
               Just x -> 
                  case (H.decodeHandshake x) of
                       Right hand -> liftIO $ print hand
                       Left xm2 -> 
                          case M.getFullMessage x of
                               k -> do liftIO $  print ("Req For "++ (show k))
                                       sink
                               
                                       
            
save :: Sink (String, BC.ByteString) IO ()            
save = awaitForever (\(x,y) -> (liftIO . print) ("Save to "++ (show x)))       
            
            
test = do 
  p <- peer 
  T.tube p source sink save













