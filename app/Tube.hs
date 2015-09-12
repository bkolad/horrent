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
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Maybe


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
     
     
     


logParsingError :: Conduit (Perhaps M.Message) IO M.Message                                
logParsingError =
  do message <- await
     case message of
           Nothing -> return ()       
           Just (Left x) -> 
              liftIO $ print ("Parsing Error " ++ (show x))
           Just (Right x) -> 
              do yield x
                 logParsingError 
           

 
class Fooable a where
   foo :: a -> String 
 
data Initialized 
data NotInitialized 
 
data P a = P Int
 
 
instance Fooable (P Initialized) where
  foo (P x) = ("Init " ++ (show x))
  
  
--instance Fooable (P NotInitialized) where
--  foo (P x) = ("NOT INIT" ++ (show x)) 
 

i :: P Initialized
i = P 3

n :: P NotInitialized
n = P 5
  
convert :: P NotInitialized -> P Initialized
convert (P x) = P x
 
 
blabla :: (Fooable a) => a -> String
blabla = foo
        
        
  
recMessage :: P.Peer -> CN.AppData -> Conduit M.Message IO P.Peer    -- TODO recurse always check req next in sinq --? -- replace 
recMessage peer appData = do
   message <- await
   case message of
        Nothing -> return ()
        Just (M.Bitfield b) -> 
           do let pList = P.convertToBits b 
                  newPeer = peer {P.pieces = pList}
              liftIO $ sendInterested appData
              recMessage newPeer appData
                 
        Just (M.Have b) -> 
           do let pList = (P.fromBsToInt b) : P.pieces peer
                  newPeer = peer {P.pieces = pList}
              recMessage newPeer appData
        
        Just M.UnChoke -> 
           do let newPeer = peer {P.unChoked = True}
              yield newPeer
              recMessage newPeer appData
        
        Just (M.Piece p@(idx,offset,content)) ->
           do let newPeer = peer {P.buffer = Just p}
              yield newPeer
              --liftIO $ print newPeer
              recMessage newPeer appData 
    
        Just M.Choke -> return ()
        
        Just y -> 
           do liftIO $ 
                 print ("This message should not arrive while downloading " ++ (show y))     
            --  return ()
        
        
   
bufferContent :: TP.Buffer -> Conduit P.Peer IO P.Peer        
bufferContent buffer =
   do mPeer <- await
      case mPeer of 
           Nothing -> return ()
           Just peer -> 
              do let pB = P.buffer peer
                     next = P.requestNext peer
                 case pB of
                      Nothing -> bufferContent buffer 
                      Just (idx, offset, buff) ->
                         do let newBuffer = buffer Seq.|> buff
                            bufferContent newBuffer
                 
              {--
   do message <- await
      case message of
           Nothing -> return () 
            
           Just (M.Bitfield b) -> 
              do 
                 let pList = P.convertToBits b 
                     newPeer = peer {P.pieces = pList}
                 liftIO $ sendInterested appData
                 recMessage newPeer appData
           
           Just (M.Have b) -> 
              do 
                 let pList = (P.fromBsToInt b) : P.pieces peer
                     newPeer = peer {P.pieces = pList}
                 recMessage newPeer appData
          
           Just M.UnChoke -> 
              do
                 nextPiece <- liftIO $ P.requestNext peer 
                 case nextPiece of
                      Nothing -> return ()
                      Just x -> 
                         do 
                            yield peer
                            liftIO $ sendRequest appData
                            recMessage peer appData
                       
           --Just (Right (M.Piece (idx,offset,content))) ->
             -- return ()
          
          
           Just M.Choke -> 
              liftIO $ print "M.Choke"
           
           Just y -> 
              do 
                 yield peer
                 liftIO $ sendRequest appData
                 recMessage peer appData --}
                 
                                                         
                            
                         
         

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
                  =$= logParsingError
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
