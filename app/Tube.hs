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


type Perhaps a = Either String a          
  
  
data PeerState = PeerState { appData :: CN.AppData
                            ,pieces :: [Int] 
                            ,infoHash :: B.ByteString}  
  
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
     
     
  
recMessage :: PeerState -> Conduit (Perhaps M.Message) IO (Perhaps String)   
recMessage peer =
   do message <- await
      case message of
           Nothing -> return ()
           Just (Left x) -> yield $ Left x
           Just (Right (M.Bitfield b)) -> 
              do 
                 let pList = convertToBits b 
                 yield (Right $ "BB " ++ (show pList))
                 recMessage $ peer {pieces = pList}
           
           Just (Right (M.Have b)) -> 
              do 
                 let pList = (fromBsToInt b) : pieces peer
                 yield (Right $ "H " ++ (show pList))
                 recMessage $ peer {pieces = pList}
          
           Just (Right y) -> 
              do 
                 yield (Right $ show y)
              --   recMessage appData
       
                     
                                                         
                            
                         
         

-- sinkH = awaitForever (liftIO . print . show)

--sinkM :: Sink (Either String String) IO ()
sinkM = awaitForever (liftIO . print . show)
   
   
generiCSource source lo =
  do yield lo
     source
  
  
-- ((addCleanup (const $ liftIO $ putStrLn "Stopping")) $ source)  
  
tube :: PeerState -> IO ()  
tube peer = 
   do sendHandshake (infoHash peer) (appData peer)
      let source = (addCleanup (const $ liftIO $ putStrLn "Stopping")) $ CN.appSource (appData peer)
      leftOver <-  source $$ recHandshake
      case leftOver of
         Left l -> print l
         Right (lo, h) -> 
              do generiCSource source lo
                  $= (flushLeftOver messageAndLeftOver) 
                  =$= (recMessage peer)
                  $$ sinkM  
                            
                            
    
    
    
messageAndLeftOver :: BC.ByteString -> (Maybe BC.ByteString, Perhaps M.Message)  
messageAndLeftOver x = 
    case (M.decodeMessage x) of     
         Left (lo, idx, errorM) -> 
              (Nothing, Left $ "PARSING ERROR " ++ errorM)    
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
                      
 
 
fromBsToInt bs = 
   sum $ zipWith (\x y->x*2^y) (reverse ws) [0,8..]
   where 
      ws = map fromIntegral (B.unpack bs)
                       
convertToBits bs = 
   map snd $ filter fst $ zip bits [0 ..] 
   where
     bits = [Bits.testBit w i| w <- B.unpack bs, i <- [7,6.. 0]] 
                      
                      
                      
sentInterested :: CN.AppData -> IO() 
sentInterested appData = yield (M.encodeMessage M.Interested) $$ CN.appSink appData  

