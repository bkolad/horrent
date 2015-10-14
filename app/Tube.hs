{-# LANGUAGE FlexibleInstances, ViewPatterns #-}
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
import qualified Crypto.Hash.SHA1 as SHA1 
import Control.Monad.Trans.Resource
import Data.Maybe
import qualified Data.Binary.Get as G

          --8192
chunkSize = 16384  

sendHandshake :: B.ByteString ->  CN.AppData -> IO ()  
sendHandshake infoHash appData = 
   yield handshake $$ CN.appSink appData 
   where      
      handshake = H.createHandshake infoHash
      
      
      
recHandshake :: Sink BC.ByteString IO (TP.Perhaps (BC.ByteString, H.Handshake)) 
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
   CN.AppData 
   -> P.Peer 
   -> Conduit M.Message IO (String, BC.ByteString)
recMessage appData peer = do
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
            liftIO $ sendInterested appData
            recMessage appData newPeer 
              
           
           
       Just (M.Have b) -> do 
            liftIO $ print "Have"  
            let pList = (P.fromBsToInt b) : P.pieces peer
                newPeer = peer {P.pieces = pList}  
            recMessage appData newPeer 
            
            
                                         
       Just M.UnChoke -> do 
            liftIO $  print "UnChoke"
            nextM <- liftIO $ requestNextAndUpdateGlobal pieces global 
            case nextM of
                 Nothing -> 
                      return ()                    
                 Just next -> do          
                      liftIO $ print ("Req "++(show next))
                      liftIO $ sendRequest appData (next, 0, chunkSize)
                      liftIO $ setStatus next global  TP.InProgress
            
                      recMessage appData peer 
                        
      
         
       Just (M.Piece (idx, offset, chunkBuffer)) -> do     
            let newBuffer = (P.buffer peer) `BC.append` chunkBuffer
                newPeer = peer {P.buffer = newBuffer}  
                size = getSize idx (P.sizeInfo peer)           
            handlePiecie appData (idx,offset) size newPeer  
     
     
      
       Just M.Choke -> 
            return ()
      
       Just M.KeepAlive -> do 
            liftIO $ print "KeepAlive" 
            recMessage appData peer 
                       
      
       Just y -> do 
            liftIO $ print ("This message should not arrive while downloading " ++ (show y))     
            return ()
                   
      
      
      
          
handlePiecie :: 
   CN.AppData
   ->(Int, Int)
   -> Int
   -> P.Peer
   -> ConduitM M.Message (String, BC.ByteString) IO ()               
handlePiecie appData (idx, offset) size peer  
  | (offset < size - chunkSize) = do
       liftIO $ print ((show idx) ++ " " ++ (show offset))    
       liftIO $ sendRequest appData (idx, offset + chunkSize , chunkSize)
      
       recMessage appData peer 
       
       
 | otherwise = do     
      let pieces = P.pieces peer
          global = P.globalStatus peer
  
      nextM <- liftIO $ requestNextAndUpdateGlobal pieces global                 
      case nextM of
            Nothing -> 
                 return ()
            Just next -> do    
                
                 liftIO $ setStatus idx (P.globalStatus peer)  TP.Done 
                 liftIO $ sendRequest appData (next, 0 , reqSize next)
                 liftIO $ print ("Next " ++ (show next))      
                 let newBuffer = P.buffer peer
                     hshEq = ((Seq.index (P.peceHashes peer) idx) == SHA1.hash newBuffer)
                 liftIO $ print hshEq
         
                 yield (show idx, newBuffer)
              
                 let newPeer = peer {P.buffer = BC.empty}
                 recMessage appData newPeer  
                
      where         
         reqSize next 
            | (last (P.pieces peer) == next) =
                 min (lastS (P.sizeInfo peer)) chunkSize
                 
            | otherwise = 
                 chunkSize      
                 
         lastS (nbOfPieces, normalSize, lastSize) = lastSize 
        
     
       
  
  
getSize next (nbOfPieces, normalSize, lastSize) 
   | (next == nbOfPieces -1) = lastSize
   | otherwise               = normalSize 
  
                                                                  
  
  
  
  
 
  
saveToFile :: Sink (String, BC.ByteString) IO ()
saveToFile = do
  awaitForever (liftIO . save)
  where 
    save :: (String, BC.ByteString) -> IO()
    save (fN, c) =
       runResourceT $ 
        (yield c) 
        $$ (CB.sinkFile ("downloads/" ++ fN))
        
  

flushLeftOver :: BC.ByteString -> Conduit BC.ByteString IO BC.ByteString
flushLeftOver lo 
   | (not . B.null) lo = do
        yield lo
        awaitForever yield     
        
   | otherwise         = awaitForever yield
  
  
                                       
-- let source = (addCleanup (const $ liftIO $ putStrLn "Stopping ---")) $ CN.appSource (appData)
    

tube :: P.Peer -> CN.AppData -> IO ()  
tube peer appData = do  
   let infoHash = P.infoHash peer
   sendHandshake infoHash appData
  
   let source = CN.appSource appData             
   (nextSource, handshake) <- source $$+ recHandshake
   
   let global     = P.globalStatus peer
       infoSize   = P.sizeInfo peer
       peceHashes = P.peceHashes peer
                
  
   case handshake of
      Left l -> 
         print l
      Right (bitFieldLeftOver, hand) -> 
         nextSource 
    
         $=+  flushLeftOver bitFieldLeftOver 
      
         =$=  decodeMessage M.getMessage
      
         =$=  recMessage appData peer 
      
         $$+- saveToFile  
     
   
    
                      
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
                      
                      
                      
sendInterested :: CN.AppData -> IO() 
sendInterested appData = 
  yield (M.encodeMessage M.Interested) 
  $$ CN.appSink appData  

  
  
sendRequest :: CN.AppData -> (Int, Int, Int) -> IO() 
sendRequest appData req = 
  yield (M.encodeMessage $ M.Request req) 
  $$ CN.appSink appData  


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
    src3 $$ conduit1 =$ sink3            
            