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
      
      
 
recHandshake :: Sink B.ByteString IO (Maybe B.ByteString) -- Chnage it to ErrorT
recHandshake = 
     do -- liftIO $ print "HANDSHAKE"
        handM <- await
        case handM of
             Nothing -> return Nothing
             Just hand -> do
                  let e = H.recvHandshakeC2 hand
                  case e of
                       Left _ -> return Nothing
                       Right (leftOver, idx, h) -> do
                               liftIO $ print $ "Handshake from " ++ (show h)
                               return $ Just $ BL.toStrict leftOver
                  
                  
 
recMessage :: Bool -> Conduit BC.ByteString IO M.Message 
recMessage b = 
    do message <- await
       case message of
            Nothing -> return ()
            Just jm -> 
                 do case (M.getMessageC jm) of
                         Left left -> do  liftIO $ print left
                                          return ()
                         Right (leftO, idx, m) -> 
                               do -- liftIO $ print $ "MESSAGE " ++ (show m)
                                  let lo = BL.toStrict leftO
                                  if (not  (B.null lo)) then
                                       do -- liftIO $ print $ "lo not null " ++ (show b) 
                                          yield m
                                          leftover $ lo
                                          recMessage b
                                  else 
                                      do yield m
                                         if b then (recMessage b) else (return ())
    

    
sink :: Sink M.Message IO ()      -- process messages here
sink = awaitForever (liftIO . print)
    
    
                  
 
tube appData = 
    do leftOver <- CN.appSource appData $$ recHandshake
       case leftOver of
            Nothing -> print "Problem with Handshake"
            Just lo -> do yield lo $= recMessage False $$ sink  
                          CN.appSource appData $= recMessage True $$ sink
  
 
 
       
