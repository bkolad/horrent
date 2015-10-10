{-# LANGUAGE FlexibleInstances, ViewPatterns #-}
module Tube where

import Data.Conduit
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
import Data.Conduit
import Data.Maybe
import qualified Data.Binary.Get as G


  

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
   
      
      
 
recMessage :: P.Peer -> CN.AppData -> Conduit M.Message IO P.Peer    
recMessage peer appData = do
  message <- await 
  case message of
       Nothing -> do liftIO $ print "NOTHING NO MESSAGE"
                     return ()
      
       Just (M.Bitfield b) ->
         do liftIO $ print "BF"
            let pList = P.convertToBits b 
                newPeer = peer {P.pieces = pList}
            liftIO $ sendInterested appData
            recMessage newPeer appData
              
       Just (M.Have b) -> 
         do liftIO $ print "Have"  
            let pList = (P.fromBsToInt b) : P.pieces peer
                newPeer = peer {P.pieces = pList}  
            recMessage newPeer appData
                                                                          -- TODO use phantom types for peer validation
       Just M.UnChoke -> 
         do 
            liftIO $  print "UnChoke"
            let newPeer = peer {P.unChoked = True}
            yield newPeer
            recMessage newPeer appData
      
       Just (M.Piece p@(idx,offset,content)) ->
         do --liftIO $  print "Piece"
            let newPeer = peer {P.buffer = Just p}
            yield newPeer
            recMessage newPeer appData 
  
       Just M.Choke -> return ()
      
       Just M.KeepAlive -> 
         do liftIO $ print "KeepAlive" 
            recMessage peer appData
      
       Just y -> 
         do liftIO $ 
              print ("This message should not arrive while downloading " ++ (show y))     
          --  return ()
      
      
   
forwardContent :: CN.AppData -> Conduit P.Peer IO (String, BC.ByteString)--P.Peer  -- Last/NotLast (Idx, offset buff)         
forwardContent appData = do 
     mPeer <- await
     case mPeer of 
          Nothing -> return ()
          Just peer -> 
            do liftIO $ print "--"
               let pB = P.buffer peer
                   global = P.globalStatus peer
                   peerPieces = P.pieces peer
                   infoSize = P.sizeInfo peer
               nextM <- liftIO $ requestNextAndUpdateGlobal peerPieces global 
               case (nextM, pB) of
                    (Nothing, Nothing) -> do
                      liftIO $ print "EXIT"
                      return () 
                    
                    (Nothing, Just (idx, offset, buff)) -> do
                      liftIO $ print $ "DONE " ++ (show idx)         
                      liftIO $ setStatus idx global TP.Done    
                      liftIO $ print $ Seq.index (P.peceHashes peer) idx == SHA1.hash buff
                    
                      yield (show idx, buff)
                  
                      return ()
                                  
                    (Just next, Nothing) -> do
                      liftIO $ print "REQ 0"
                      let size = getSize next infoSize
                      liftIO $ sendRequest appData (0, 0, size)
                      liftIO $ setStatus next global TP.InProgress
                      forwardContent appData
                             
                    (Just next, Just (idx, offset, buff)) -> do      
                   --   liftIO $ print "GOT"
                      let size = getSize next infoSize
                      liftIO $ print ( (show idx) ++" arrived "++(show(B.length buff)))
                      liftIO $ print $ Seq.index (P.peceHashes peer) idx == SHA1.hash buff
                      
                      liftIO $ setStatus idx global TP.Done
                      
                      
                      liftIO $ sendRequest appData (next, 0, size)
                      yield (show idx, buff)
                                       
                      forwardContent appData 

                                                            
  
 
  
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
  
   case handshake of
      Left l -> 
         print l
      Right (bitFieldLeftOver, hand) -> 
         nextSource 
    
         $=+  flushLeftOver bitFieldLeftOver 
      
         =$=  decodeMessage M.getMessage
      
         =$=  recMessage peer appData
      
         =$=  forwardContent appData
      
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
                      

                      
   
 
getSize next (nbOfPieces, normalSize, lastSize) 
   | (next == nbOfPieces -1) = lastSize
   | otherwise               = normalSize 
  

  
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

