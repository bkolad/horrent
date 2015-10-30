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

bs = BC.replicate chunkSize '1'

hashP = P.hashFor bs


peer :: IO P.Peer
peer = do
  global <- (TP.newGlobalBitField 100)
  return $ P.Peer "Host!" 0 [] (BC.pack  hash) global True (BC.empty) (TP.hashInfoFromList [hashP, hashP, hashP])  (3, 16384, 11)


  
source :: Source IO BC.ByteString
source = do yield $ H.createHandshake (BC.pack hash) 
            yield $ M.encodeMessage $ M.Bitfield (P.hasIdxs [1,2,9])
            yield $ M.encodeMessage $ M.UnChoke
            yield $ M.encodeMessage $ M.Piece(1, 0, bs)
            yield $ M.encodeMessage $ M.Piece(2, 0, bs)
          
            
            
                           
sink :: Sink BC.ByteString IO ()
sink = do xm <- await
          case xm of 
               Nothing -> return ()
               Just x -> 
                  case (H.decodeHandshake x) of
                       Right hand -> liftIO $ print hand
                       Left xm2 -> 
                          case M.getFullMessage x of
                               Right (_, _, M.Interested) -> 
                                    do liftIO $ print "Interested"
                                       sink
                               
                               k -> do liftIO $  print k
                                       sink
                               
                                       
            
save :: Sink (String, BC.ByteString) IO ()            
save = awaitForever (\(x,y) -> (liftIO . print) x)        
            
            
test = do 
  p <- peer 
  T.tube p source sink save













