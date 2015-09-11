module Tube where

import Data.Conduit
import qualified Message as M
import Control.Monad.IO.Class
import qualified Handshake as H
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import qualified Data.Conduit.Network as CN
import Control.Monad.Trans.Resource
import qualified Data.Bits as Bits
import qualified Peer as P
import qualified Control.Concurrent as CC


type Perhaps a = Either String a          
  
  
sendHandshake :: B.ByteString ->  CN.AppData -> IO ()  
sendHandshake infoHash appData = 
   yield handshake $$ CN.appSink appData 
   where      
      handshake = H.createHandshake infoHash
      
    
      
recHandshake :: Monad m => Sink BC.ByteString m (Perhaps (BC.ByteString, H.Handshake)) -- <-- replace IO by Either
recHandshake = 
   do handM <- await
      case handM of
           Nothing -> return $ Left "No handshake"                               
           Just hand -> return $ convertHandshake hand
      where 
            convertHandshake = convertParsedOutput . H.decodeHandshake 
            convertParsedOutput x = 
               do case x of
                       Left (_, _, e) -> Left $ "Problem with parsing handshake " ++ (show e)
                       Right (leftOver, _, h) -> Right $ (BL.toStrict leftOver,  h)
     
     
     
-- TODO forever Either retray     

foreverE :: (M.Message -> Conduit (Perhaps M.Message) IO M.Message) -> Conduit (Perhaps M.Message) IO M.Message 
foreverE fun =
   do message <- await
      case message of
           Nothing -> return () 
           Just (Left x) -> 
              do 
                 liftIO $ print "logError" -- Or Yield
                 return ()
           Just (Right x) -> do fun x
                                foreverE fun
            

  
recMessage :: P.Peer -> CN.AppData -> Conduit (Perhaps M.Message) IO (Perhaps String)   
recMessage peer appData =
   do message <- await
      case message of
           Nothing -> return () 
                  
           Just (Left x) ->
              do 
                 yield $ Left x
                 liftIO $ CC.threadDelay 1000000
                 liftIO $ sendRequest appData
                 recMessage peer appData
            
           Just (Right (M.Bitfield b)) -> 
              do 
                 let pList = P.convertToBits b 
                     newPeer = peer {P.pieces = pList}
                 liftIO $ sendInterested appData
                 recMessage newPeer appData
           
           Just (Right (M.Have b)) -> 
              do 
                 let pList = (P.fromBsToInt b) : P.pieces peer
                     newPeer = peer {P.pieces = pList}
                 recMessage newPeer appData
          
           Just (Right M.UnChoke) -> 
              do 
                 nextPiece <- liftIO $ P.requestNext peer 
                 case nextPiece of
                      Nothing -> return ()
                      Just x -> 
                         do 
                            yield (Right $ show M.UnChoke)
                            liftIO $ sendRequest appData
                            recMessage peer appData
                       
           --Just (Right (M.Piece (idx,offset,content))) ->
             -- return ()
          
          
           Just (Right M.Choke) -> 
              liftIO $ print "M.Choke"
           
           Just (Right y) -> 
              do 
                 yield (Right $ show y)
                 liftIO $ sendRequest appData
                 recMessage peer appData
                 
                                                         
                            
                         
         

-- sinkH = awaitForever (liftIO . print . show)

--sinkM :: Sink (Either String String) IO ()
sinkM = awaitForever (liftIO . print . show)
   
   
generiCSource source lo =
  do yield lo
     source
  
  
-- ((addCleanup (const $ liftIO $ putStrLn "Stopping")) $ source)  
  
tube :: P.Peer -> CN.AppData -> IO ()  
tube peer appData = 
   do sendHandshake (P.infoHash peer) appData
      let source = (addCleanup (const $ liftIO $ putStrLn "Stopping")) $ CN.appSource (appData)
      leftOver <-  source $$ recHandshake
      case leftOver of
         Left l -> print l
         Right (lo, h) -> 
              do generiCSource source lo
                  $= (flushLeftOver messageAndLeftOver) 
                  =$= (recMessage peer appData)
                  $$ sinkM  
                            
                            
    
    
messageAndLeftOver :: BC.ByteString -> (Maybe BC.ByteString, Perhaps M.Message)  
messageAndLeftOver x = 
    case (M.decodeMessage x) of     
         Left (lo, idx, errorM) -> 
              (Nothing, Left $ "HORRENT PARSING ERROR: " ++(show lo) ++" "++(show idx)++" " ++ errorM)    
         Right (lo, idx, m) -> do
              if ((not . BL.null) lo) 
                 then ((Just $ BL.toStrict lo), Right m)
                 else (Nothing, Right m)    

                  
 
-- Combinator 
flushLeftOver :: Monad m => (r -> ((Maybe r), k)) -> Conduit r m k  
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

sendRequest :: CN.AppData -> IO() 
sendRequest appData = yield (M.encodeMessage $ M.Request (0,0,32)) $$ CN.appSink appData  
