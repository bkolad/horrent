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
     do let handshake = (H.createHandshake infoHash)
        yield handshake $$ CN.appSink appData 
      
      
 
 
 
recHandshake :: Sink BC.ByteString IO (Maybe BC.ByteString) -- Chnage it to ErrorT
recHandshake = 
     do -- liftIO $ print "HANDSHAKE"
        handM <- await
        case handM of
             Nothing -> return Nothing                                  -- <- map from conduit
             Just hand -> do
                  let e = H.recvHandshakeC2 hand
                  case e of
                       Left _ -> return Nothing
                       Right (leftOver, idx, h) -> do
                               liftIO $ print $ "Handshake from " ++ (show h)
                               return $ Just $ BL.toStrict leftOver
  
  
  
messageAndLeftOver :: BC.ByteString -> (Maybe BC.ByteString, Either String M.Message)  
messageAndLeftOver m = 
    case (M.getMessageC m) of
         Left (lo, idx, errorM) -> 
              (Nothing, Left $ "PARSING ERROR " ++ errorM) 
              
         Right (lo, idx, m) -> do
               if ((not . BL.null) lo) 
                  then ((Just $ BL.toStrict lo), Right m)
                  else (Nothing, Right m)
              
  
  
 
--recM :: Conduit M.Message IO M.Message   
recM =
    do message <- await
       case message of
            Nothing -> return ()
            Just jm -> do yield jm 
                          recM
         

    
--sink :: Sink M.Message IO ()      -- process messages here
sink = awaitForever (liftIO . print)
    

tube appData = 
    do leftOver <- CN.appSource appData $$ recHandshake
       case leftOver of
            Nothing -> print "Problem with Handshake"
            Just lo -> do yield lo $= (flushLeftOver False messageAndLeftOver) =$= recM $$ sink  
                          CN.appSource appData 
                            $= (flushLeftOver True messageAndLeftOver) 
                            =$= recM 
                            $$ sink
    
    
                  
 
-- Combinator 
flushLeftOver :: Monad m => Bool ->  (r -> ((Maybe r), k)) -> Conduit r m k  
flushLeftOver forever fun = awaitForever (process fun)
    where   
         process fun f =  
              do let (loM, k) = fun f
                 yield k    
                 case loM of 
                      Nothing -> if forever then 
                                    (flushLeftOver forever fun) 
                                 else return ()
                      Just lo -> leftover lo 
       
