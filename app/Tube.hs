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
  
  
sendHandshake :: B.ByteString ->  CN.AppData -> IO ()  
sendHandshake infoHash appData = 
   do let handshake = H.createHandshake infoHash
      yield handshake $$ CN.appSink appData 
      

      
      
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
        
  
 

sentInterested :: CN.AppData -> IO() 
sentInterested appData = yield (M.encodeMessage M.Interested) $$ CN.appSink appData  



                      
fromBsToInt bs = sum $ zipWith (\x y->x*2^y) (reverse ws) [0,8..]
                 where ws = map fromIntegral (B.unpack bs)
                       
convertToBits bs = 
  let bits = [Bits.testBit w i| w<-B.unpack bs, i<-[7,6.. 0]]
  in map snd $ filter fst $ zip bits [0 ..]
                   
  
recMessage :: CN.AppData -> Conduit (Perhaps M.Message) IO (Perhaps String)   
recMessage appData =
   do message <- await
      case message of
           Nothing -> return ()
           Just (Left x) -> yield $ Left x
           Just (Right (M.Bitfield b)) -> do  
                                       yield (Right $ "BB " ++ (show (convertToBits b)))
                                       recMessage appData
           
           Just (Right (M.Have b)) -> do  
                                       yield (Right $ "H " ++ (show (fromBsToInt b)))
                                       recMessage appData
          
          
           Just (Right y) -> do 
                                yield (Right $ show y)
                                recMessage appData
          
           {-Just jm -> do case jm of 
                              Left x -> yield $ Left x
                              Right (M.Bitfield b) -> do
                                                         liftIO $ sentInterested appData
                                                         yield $ Right "B"  -- sequence                          
                                                         recMessage appData
                              Right (M.Have b) -> do 
                                                      yield $ Right $ "H"                           
                                                      recMessage appData
                                                      
                              Right (M.KeepAlive) -> do 
                                                         yield $ Right $ "K"                           
                                                         recMessage appData  
                              Right (M.UnChoke) -> do 
                                                         yield $ Right $ "U!!"                           
                                                         recMessage appData          -}                
                                                         
                            
                         
         

-- sinkH = awaitForever (liftIO . print . show)

--sinkM :: Sink (Either String String) IO ()
sinkM = awaitForever (liftIO . print . show)
      
tube appData = 
   do leftOver <- CN.appSource appData $$ recHandshake
      case leftOver of
         Left l -> print l
         Right (lo, h) -> 
              do yield lo 
                  $= (flushLeftOver False messageAndLeftOver) 
                  =$= (recMessage appData)
                  $$ sinkM  
                  
                 CN.appSource appData 
                  $= (flushLeftOver True messageAndLeftOver) 
                  =$= (recMessage appData) 
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
flushLeftOver :: Monad m => Bool ->  (r -> ((Maybe r), k)) -> Conduit r m k  
flushLeftOver forever fun = awaitForever $ process fun
    where   
          process fun f =  
              do let (loM, k) = fun f
                 yield k    
                 case loM of 
                      Nothing -> if forever then  -- GUARD
                                    (flushLeftOver forever fun) 
                                 else return ()
                      Just lo -> leftover lo 
                      
                      
                      
                      
{--  
processMessageC :: CN.AppData ->  Conduit (Either String M.Message) IO (Either String String)  
processMessageC appData = awaitForever (\x -> do lo <- liftIO $ processMessage appData x
                                                 yield $ lo )  

processMessage :: CN.AppData -> M.Message -> IO String
processMessage appData m = 
  do case m of
          M.Bitfield b -> do sendInterested appData
                             return "BIT FIELD"
          M.Have h -> return "Have"
          _ -> return "Bla bla"
          --}
                      
       
