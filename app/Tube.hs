{-# LANGUAGE FlexibleInstances, InstanceSigs, LiberalTypeSynonyms #-}
module Tube where

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
           
             
 
decodeMessage :: G.Decoder M.Message -> Conduit BC.ByteString IO M.Message  
decodeMessage dec = do
   case dec of 
      (G.Fail _ _ _) -> 
         liftIO $ print "ERROR: DECODING FAILURE"   
      
      G.Partial fun -> 
         await >>= maybe (return ()) 
                         (\x -> decodeMessage $ fun (Just x))   
    
      (G.Done lo _ x) -> do
         yield x
         leftover lo
         (decodeMessage M.getMessage)
   
      
      
 
recMessage ::  
   Sink BC.ByteString IO () 
   -> P.Peer 
   -> Conduit M.Message IO (String, BC.ByteString)
recMessage peerSink peer = do
  message <- await 
  
  let pieces = P.pieces peer
      global = P.globalStatus peer
            
  case message of
       Nothing -> do 
            return ()
         
      
       Just (M.Bitfield b) -> do 
            liftIO $ print "BF"
            let pList = P.convertToBits b 
                newPeer = peer {P.pieces = pList}
            liftIO $ sendInterested peerSink
            recMessage peerSink newPeer 
              
           
           
       Just (M.Have b) -> do 
            liftIO $ print "Have"  
            let pList = (P.fromBsToInt b) : P.pieces peer
                newPeer = peer {P.pieces = pList}  
            recMessage peerSink newPeer 
            
            
                                         
       Just M.UnChoke -> do 
            liftIO $  print "UnChoke"
            nextM <- liftIO $ requestNextAndUpdateGlobal pieces global 
            case nextM of
                 Nothing -> 
                      return ()                    
                 Just next -> do          
                      liftIO $ print ("Req "++(show next))
                      liftIO $ sendRequest peerSink (next, 0, chunkSize)
                      liftIO $ setStatus next global  TP.InProgress
            
                      recMessage peerSink peer 
                        
      
         
       Just (M.Piece (idx, offset, chunkBuffer)) -> do     
            let newBuffer = (P.buffer peer) `BC.append` chunkBuffer
                newPeer = peer {P.buffer = newBuffer}  
                size = getSize idx (P.sizeInfo peer)  
                
            -- liftIO $ print $ "+                         M.Piece " ++ (show (idx, offset, BC.length newBuffer))
                
                
                
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
            liftIO $ print "KeepAlive" 
            recMessage peerSink peer 
                       
      
       Just y -> do 
            liftIO $ print ("This message should not arrive while downloading " ++ (show y))     
            return ()
                   
      
      
          
          
handlePiecie :: 
   Sink BC.ByteString IO ()
   ->(Int, Int)
   -> Int
   -> P.Peer
   -> ConduitM M.Message (String, BC.ByteString) IO Exec               
handlePiecie peerSink (idx, offset) pieceSize peer  
  | (offset < sizeLeft) = do
    
       liftIO $ print ((show idx) ++ " " ++ (show offset) ++" "++ (show (BC.length (P.buffer peer))))    
       liftIO $ sendRequest peerSink (idx, offset + chunkSize , min sizeLeft chunkSize) 
   
       return Continue
       
       
       
 | otherwise = do     
      let pieces = P.pieces peer
          global = P.globalStatus peer
          newBuffer = P.buffer peer
          hshEq = ((Seq.index (P.peceHashes peer) idx) == P.hashFor newBuffer)
     
      liftIO $ print $ "HashEQ "++ (show hshEq) ++ " "++ (show (BC.length newBuffer))
      liftIO $ print $ ""
             
  
      nextM <- liftIO $ requestNextAndUpdateGlobal pieces global                 
      case nextM of
            Nothing -> do
                 liftIO $ print "EXIT"
                 return LastPiece 
                 
            Just next -> do    
                 liftIO $ print ("Next " ++ (show next))     
                 liftIO $ setStatus idx (P.globalStatus peer)  TP.Done 
                 liftIO $ sendRequest peerSink (next, 0 , reqSize next)
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
  
                                                                  
  
   

flushLeftOver :: BC.ByteString -> Conduit BC.ByteString IO BC.ByteString
flushLeftOver lo 
   | (not . B.null) lo = do
        yield lo
        awaitForever yield     
        
   | otherwise         = awaitForever yield
  
  
                                       
-- let source = (addCleanup (const $ liftIO $ putStrLn "Stopping ---")) $ CN.appSource (appData)




tube :: 
   P.Peer
   -> ConduitM () BC.ByteString IO () --Source IO BC.ByteString 
   -> Sink BC.ByteString IO ()
   -> Sink (String, BC.ByteString) IO ()
   -> IO ()  
tube peer getFrom sendTo saveTo = do  
   let infoHash = P.infoHash peer
   sendHandshake infoHash sendTo
  
   (nextSource, handshake) <- getFrom $$+ recHandshake
    
   let global     = P.globalStatus peer
       infoSize   = P.sizeInfo peer
       peceHashes = P.peceHashes peer
                
   case handshake of
      Left l -> 
         print $ "Bad Handshake : " ++l
         
      Right (bitFieldLeftOver, hand) -> 
         nextSource 
    
         $=+  flushLeftOver bitFieldLeftOver 
      
         =$=  decodeMessage M.getMessage
      
         =$=  recMessage sendTo peer 
      
         $$+- saveTo  
     
   
   
    
                      
                      
                      
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


            
{--            
            
main :: IO ()
main = do
    let src = mapM_ yield [1..3 :: Int]
        src2 = mapM_ yield [8..10 :: Int]
        src3 = getZipConduit $ ZipConduit src <* ZipConduit src2
        conduit1 = CL.map (+1)
     --   conduit2 = CL.concatMap (replicate 2)
     --   conduit = getZipConduit $ ZipConduit conduit1 <* ZipConduit conduit2
        sink = CL.mapM_ print
        sink1 = CL.mapM_ (\x -> print ("lala "++(show x)))
        sink3 = getZipConduit $ ZipConduit sink <* ZipConduit sink1
        src3 $$ conduit1 =$ sink3        --}   
            
            
            
            
            
            
 
 

                                                       
                             
class  Logger l where
  logg ::  String -> l String
  

instance Logger IO where
  logg a =  do liftIO $ print a
               return a
               

instance Logger (WT.Writer [String]) where
  logg a =  (WT.writer (a, [a]))               
               
               


src :: (Monad l, Logger l) => Source l Int
src = do Trans.lift $ logg "xxx" 
         yield 1
         yield 2
         
         
         
si :: (Monad l, Logger l) => Sink Int l ()
si = do
        xM <- await
        case xM of
              Nothing -> do
                Trans.lift $ logg "aaa" 
                return ()
         
              Just x -> do
              si 
          

kk :: IO ()      
kk = (src $$ si)         
      
ll :: ((), [String])      
ll = runIdentity $ WT.runWriterT (src $$ si)         
               