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


       
  
sendHandshake :: B.ByteString ->  CN.AppData -> IO ()  
sendHandshake infoHash appData = 
   do let handshake = H.createHandshake infoHash
      yield handshake $$ CN.appSink appData 
      
      
 
 
recHandshake :: Sink BC.ByteString IO (Either String (BC.ByteString, H.Handshake)) -- <-- replace IO by Either
recHandshake = 
   do handM <- await
      case handM of
         Nothing -> 
              return $ Left "No handshake"                               
         Just hand -> do
              case H.decodeHandshake hand of   -- ideally return H.decodeHandshake hand
                 Left (_, _, e) -> 
                      return $ Left $ "Problem with handshake " ++ e    -- <-- return it from H.Handshake and use lifting                 
                 Right (leftOver, idx, h) -> do
                      liftIO $ print $ "Handshake from " ++ (show h)
                      return $ Right $ (BL.toStrict leftOver,  h)   
  
  
  
-- awaitForeverLiftEiher fun = awaitForever (\x -> yoeld $ fun <$> x)  
  
  
--recM :: Conduit M.Message IO M.Message   
recMessage =
   do message <- await
      case message of
         Nothing -> return ()
         Just jm -> do yield jm                           
                       recMessage
         

    
--sink :: Sink M.Message IO ()      -- process messages here
sink = awaitForever (liftIO . print)
    

    
tube appData = 
   do leftOver <- CN.appSource appData $$ recHandshake
      case leftOver of
         Left l -> print l
         Right (lo, h) -> 
              do yield lo 
                  $= (flushLeftOver False messageAndLeftOver) 
                  =$= recMessage 
                  $$ sink  
                  
                 CN.appSource appData 
                  $= (flushLeftOver True messageAndLeftOver) 
                  =$= recMessage 
                  $$ sink
                            
                            
    
    
    
messageAndLeftOver :: BC.ByteString -> (Maybe BC.ByteString, Either String M.Message)  
messageAndLeftOver m = 
    case (M.decodeMessage m) of
         Left (lo, idx, errorM) -> 
              (Nothing, Left $ "PARSING ERROR " ++ errorM)    -- This should be returned by get message
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
                      Nothing -> if forever then 
                                    (flushLeftOver forever fun) 
                                 else return ()
                      Just lo -> leftover lo 
                      
                      
                      
                      

                      
       
