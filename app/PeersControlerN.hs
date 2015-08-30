
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module PeersControlerN where

import qualified BencodeParser as BP (infoHash)
import qualified Connector as CN (liftEither, makePeers, getInfoHash)
import Control.Monad.Except (ExceptT, liftIO, runExceptT)
import qualified Peer as P
import qualified Network as N
import qualified StaticQueue as SQ
import qualified Data.Conduit.Network as CN
-- import qualified Data.Conduit.Combinators as CC
import Data.Conduit
import Data.Tuple (swap)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Handshake as H
import Control.Monad.IO.Class
import Data.Streaming.Network (HasReadWrite)
import qualified Control.Concurrent.Async as Async
import Control.Monad.Trans.Resource
import qualified Message as M
import qualified Data.ByteString.Lazy as BL


main::IO() 
main = do result <- runExceptT $ start "tom.torrent" 
          print result
        


start :: String -> ExceptT String IO [(N.HostName, N.PortNumber)]
start tracker = 
     do ipsPorts <-  CN.makePeers tracker
        let ipsAndPorts = (\(s, ip)-> (BC.pack s, fromIntegral ip)) <$> ipsPorts
        info <- CN.getInfoHash tracker
        liftIO $ print ipsAndPorts
        liftIO $ runClient (last ipsAndPorts) info --Async.mapConcurrently (\x -> runClient x info ) ipsAndPorts
        return ipsPorts
        
 
 
runClient :: (B.ByteString, Int) -> B.ByteString -> IO ()
runClient ipAndPort infoHash = 
    CN.runTCPClient ((uncurry CN.clientSettings) . swap $ ipAndPort) $ \appData -> do
        sendAndRecHanshake infoHash appData
   --     runResourceT $ CN.appSource appData $$ getMessage
        
sendAndRecHanshake :: BC.ByteString -> CN.AppData -> IO ()        
sendAndRecHanshake infoHash appData =
    do let handshake = (H.createHandshake infoHash)
       yield handshake $$ CN.appSink appData
       runResourceT $ CN.appSource appData $$ sinkH
                             

sinkH :: Sink B.ByteString (ResourceT IO) ()
sinkH = 
    do x <- await
       case x of
            Just k -> do let e = H.recvHandshakeC2 k
                         case e of
                              Left _ -> return ()
                              (Right (leftOver, x, han)) -> 
                                   do liftIO $ print han
                                      let w =  M.getMessageC $ BL.toStrict leftOver
                                      case w of 
                                           Left _ -> return ()
                                           (Right (leftOver2, x2, bf)) -> do liftIO $ print bf 
                                                                             let hh = M.getMessageC $ BL.toStrict leftOver2
                                                                             liftIO $ print hh
                                                                             return ()
            _ -> do 
                   liftIO $ print "END"
                   return ()                    
  
  
  
getMessage :: Sink B.ByteString (ResourceT IO) ()       
getMessage =
    do x <- await 
       case x of 
            Just k -> do liftIO $ print $ M.getMessageC k
                         getMessage
            _ -> do 
                   liftIO $ print "END"
                   return ()             
                         