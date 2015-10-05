{-# LANGUAGE FlexibleInstances #-}
module Tube where

import Data.Conduit
import qualified Message as M
import Control.Monad.IO.Class
import qualified Handshake as H
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit.Network as CN
import qualified Data.Bits as Bits
import qualified Peer as P
import qualified Control.Concurrent as CC
import qualified Data.Sequence as Seq
import qualified Types as TP
import qualified Data.Array.MArray as MA
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Maybe


type Perhaps a = Either String a          
  

sendHandshake :: B.ByteString ->  CN.AppData -> IO ()  
sendHandshake infoHash appData = 
   yield handshake $$ CN.appSink appData 
   where      
      handshake = H.createHandshake infoHash
      
      
recHandshake :: Sink BC.ByteString IO (Perhaps (BC.ByteString, H.Handshake)) -- <-- replace IO by Either
recHandshake = 
   do handM <- await
      liftIO $ print $ "GOTHS LL  " ++ (show(length handM))
      case handM of
           Nothing -> return $ Left "No handshake"    
           Just hand -> return $ convertHandshake hand
          where 
            convertHandshake = convertParsedOutput . H.decodeHandshake 
            convertParsedOutput x = 
               do case x of
                       Left (_, _, e) -> Left $ "Problem with parsing handshake " ++ (show e)
                       Right (leftOver, _, h) ->  Right $ (BL.toStrict leftOver,  h)
     
     
     
logParsingError :: Conduit (Perhaps M.Message) IO M.Message                                
logParsingError =
  do message <- await
     case message of
           Nothing -> return ()       
           Just (Left x) -> do
              liftIO $ print ("Parsing Error " ++ (show x))
              logParsingError
           Just (Right x) -> 
              do yield x
                 logParsingError 
           

 
recMessage :: P.Peer -> CN.AppData -> Conduit M.Message IO P.Peer    -- TODO recurse always check req next in sinq --? -- replace 
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
         do liftIO $  print "Piece"
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
      
      
   
forwardContent :: CN.AppData -> Conduit P.Peer IO P.Peer  -- Last/NotLast (Idx, offset buff)         
forwardContent appData =
  do -- liftIO $ print "Expecting BF"
     mPeer <- await
     case mPeer of 
          Nothing -> return ()
          Just peer -> 
            do liftIO $ print "--"
               let pB = P.buffer peer
                   global = P.globalStatus peer
                   peerPieces = P.pieces peer
                   infoSize = P.sizeInfo peer
               next <- liftIO $ requestNextAndUpdateGlobal peerPieces global 
               case (next, pB) of
                    (Nothing, Nothing) -> do
                      liftIO $ print "EXIT"
                      return () 
                    
                    (Nothing, Just (idx, offset, buff)) -> do
                      liftIO $ print $ "DONE " ++ (show idx)         
                      liftIO $ setStatus idx global TP.Done             
                      yield peer
                      return ()
                                  
                    (Just next, Nothing) -> do
                      liftIO $ print "REQ 0"
                      let size = getSize next infoSize
                      liftIO $ sendRequest appData (0, 0, size)
                      liftIO $ setStatus next global TP.InProgress
                      forwardContent appData
                             
                    (Just next, Just (idx, offset, buff)) -> do      
                      liftIO $ print "GOT"
                      let size = getSize next infoSize
                      liftIO $ print ( (show idx) ++" arrived "++(show(B.length buff)))
                      liftIO $ setStatus idx global TP.Done
                      liftIO $ sendRequest appData (next, 0, size)
                      yield peer
                  
                      
                      forwardContent appData 

printArray :: TP.GlobalPiceInfo -> IO()                      
printArray global = do 
  k <- STM.atomically $ MA.getElems global
  print $ zip k [0..]

                                                            

getSize next (nbOfPieces, normalSize, lastSize) =
  if (next == nbOfPieces -1)
     then lastSize
     else normalSize
  
  
setStatusDone :: Int -> TP.GlobalPiceInfo -> IO()
setStatusDone x global = 
  STM.atomically $ MA.writeArray global x TP.Done 


  
setStatus :: Int -> TP.GlobalPiceInfo -> TP.PiceInfo -> IO()
setStatus x global status = 
  STM.atomically $ MA.writeArray global x status 
  
  
requestNextAndUpdateGlobal :: [Int] -> TP.GlobalPiceInfo -> IO (Maybe Int)
requestNextAndUpdateGlobal pics global =
  STM.atomically $ reqNext pics global
    where
      reqNext :: [Int] -> TP.GlobalPiceInfo -> STM.STM (Maybe Int)  
      reqNext [] _ = return Nothing     
      reqNext (x:xs) global = 
        do pInfo <- MA.readArray global x
           case pInfo of
                TP.NotHave -> 
                    do MA.writeArray global x TP.InProgress 
                       return $ Just x
                TP.InProgress -> reqNext xs global
                TP.Done -> reqNext xs global

                    
                            
                      
sinkM :: Sink P.Peer IO ()
sinkM = awaitForever (\p -> liftIO $ 
  do  print "sinkM"
      printArray (P.globalStatus p))
   
   
generiCSource source lo =
  do yield lo
     source
  
  
-- ((addCleanup (const $ liftIO $ putStrLn "Stopping")) $ source)  
 
 
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


tube :: P.Peer -> CN.AppData -> IO ()  
tube peer appData = 
   do liftIO $ print "ENTER"
      sendHandshake (P.infoHash peer) appData
      let source = (addCleanup (const $ liftIO $ putStrLn "Stopping ---")) $ CN.appSource (appData)
                 
      (s1, res) <- source $$+ recHandshake
      
      let ss = case res of
                    Left l -> message (B.empty)
                    Right (lo, h) -> message lo
                    
      let cc = ss
               =$= logMSG
               =$= decodeMessage (B.empty)-- (flushLeftOver messageAndLeftOver) 
               =$= logParsingError 
               =$= (recMessage peer appData)
               =$= (forwardContent appData) 
          
      (s1 $=+ cc) $$+- sinkM 
        

message :: BC.ByteString -> Conduit BC.ByteString IO BC.ByteString        
message lo = do
           if ((not . B.null) lo)
              then do yield lo    
                      message (B.empty)
              else do k <- await
                      case k of
                           Nothing -> return ()
                           Just b -> do yield b
                                        message (B.empty)
                                        
                      
                           
  
decodeMessage ::B.ByteString ->  Conduit BC.ByteString IO (Perhaps M.Message)
decodeMessage buff= do
  xM <- await 
  case xM of
       Nothing -> return ()
       Just x -> do let nB = B.append buff x
                    case (M.decodeMessage nB ) of     
                         Left (lo, idx, errorM) ->  decodeMessage nB           -- TODO match on partial parser
                         Right (lo, idx, m) -> do
                                                  yield (Right m)
                                                  decodeMessage B.empty
      

   
 
                      
                      
                      
sendInterested :: CN.AppData -> IO() 
sendInterested appData = yield (M.encodeMessage M.Interested) $$ CN.appSink appData  

sendRequest :: CN.AppData -> (Int, Int, Int)-> IO() 
sendRequest appData req = yield (M.encodeMessage $ M.Request req) $$ CN.appSink appData  
