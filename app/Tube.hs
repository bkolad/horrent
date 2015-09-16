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
           Nothing -> do liftIO $ print "BADHS" 
                         return $ Left "No handshake" 
                         
           Just hand -> do liftIO $ print "HSOK" 
                           return $ convertHandshake hand
      where 
            convertHandshake = convertParsedOutput . H.decodeHandshake 
            convertParsedOutput x = 
               do case x of
                       Left (_, _, e) -> Left $ "Problem with parsing handshake " ++ (show e)
                       Right (leftOver, _, h) -> 
                         do Right $ (BL.toStrict leftOver,  h)
     
     
     
    
  
  
{-  
recHandshake :: Bool ->Sink BC.ByteString IO (Perhaps (BC.ByteString, H.Handshake)) -- <-- replace IO by Either
recHandshake b = 
   do handM <- await
      liftIO $ print $ "GOTHS LL  " ++ (show(length handM))
      
          
      case handM of
           Nothing -> do liftIO $ print "BADHS" 
                         return $ Left "No handshake" 
                         recHandshake True
                         
           Just hand -> do liftIO $ print "HSOK" 
                        --   return $ convertHandshake hand
                           if b 
                              then liftIO $ print $ M.decodeMessage hand
                              else return ()
 
                           recHandshake True
      where 
            convertHandshake = convertParsedOutput . H.decodeHandshake 
            convertParsedOutput x = 
               do case x of
                       Left (_, _, e) -> Left $ "Problem with parsing handshake " ++ (show e)
                       Right (leftOver, _, h) -> 
                         do Right $ (BL.toStrict leftOver,  h)
     
     
--}     


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
 
  let time = 100000
  case message of
       Nothing -> do liftIO $ print "NOTHING"
                     return ()
      
       Just (M.Bitfield b) ->
         do liftIO $ print "BF"
            let pList = P.convertToBits b 
                newPeer = peer {P.pieces = pList}
            liftIO $ CC.threadDelay time
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
            liftIO $ CC.threadDelay time
            recMessage newPeer appData
      
       Just (M.Piece p@(idx,offset,content)) ->
         do liftIO $  print "Piece"
            let newPeer = peer {P.buffer = Just p}
            yield newPeer
            liftIO $ CC.threadDelay time
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
  do liftIO $ print "Expecting BF"
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
                      -- yield Last(idx, offset, buff)
                      liftIO $ setStatus idx global TP.Done
                      return ()
                                  
                    (Just next, Nothing) -> do
                      liftIO $ print "REQ 0"
                      let size = getSize next infoSize
                      liftIO $ CC.threadDelay 100000
                      liftIO $ sendRequest appData (0, 0, 1)
                      liftIO $ setStatus next global TP.InProgress
                      forwardContent appData
                             
                    (Just next, Just (idx, offset, buff)) -> do      
                      liftIO $ print "GOT"
                      let size = getSize next infoSize
                      liftIO $ print ( (show idx) ++" arrived "++(show(B.length buff)))
                      liftIO $ CC.threadDelay 100000
                      
                      liftIO $ sendRequest appData (0, 0, 16384)
                      
                      -- 
                        -- sendReq x
                        -- global x Requested
                        -- if x > idx, offset
                              -- global idx Done
                              --yield Last
                      --       else
                              --yield (idx, offset, buff)
                      
                      forwardContent appData 


getSize next  (nbOfPieces, normalSize, lastSize) =
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
sinkM = awaitForever (liftIO . print . show)
   
   
generiCSource source lo =
  do yield lo
     source
  
  
-- ((addCleanup (const $ liftIO $ putStrLn "Stopping")) $ source)  
 
 
logMSG :: Conduit BC.ByteString IO BC.ByteString   
logMSG = do
  liftIO $ print "GOT MSG"
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
               =$= (flushLeftOver messageAndLeftOver) 
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
                                        
                      
                           
  
  
    
messageAndLeftOver :: BC.ByteString -> (Maybe BC.ByteString, Perhaps M.Message)  
messageAndLeftOver x = do
    case (M.decodeMessage x) of     
         Left (lo, idx, errorM) -> 
              (Nothing, Left $ "HORRENT PARSING ERROR: " ++(show(BL.length lo)) ++" "++(show(B.length x)) ++" "++(show idx)++" " ++ errorM)    
         Right (lo, idx, m) -> do
              if ((not . BL.null) lo) 
                 then ((Just $ BL.toStrict lo), Right m)
                 else (Nothing, Right m)    

                  
 
-- Combinator 
flushLeftOver :: (BC.ByteString -> ((Maybe BC.ByteString), k)) -> Conduit BC.ByteString IO k  
flushLeftOver fun = awaitForever $ process fun
    where   
          process fun f =  
              do let (loM, k) = fun f
                 yield k    
                 case loM of 
                      Nothing -> flushLeftOver fun 
                      Just lo -> leftover lo 
                      
 
 
 
                      
                      
                      
sendInterested :: CN.AppData -> IO() 
sendInterested appData = yield (M.encodeMessage M.Interested) $$ CN.appSink appData  

sendRequest :: CN.AppData -> (Int, Int, Int)-> IO() 
sendRequest appData req = yield (M.encodeMessage $ M.Request req) $$ CN.appSink appData  
