
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

import qualified Tube as T


main::IO() 
main = do result <- runExceptT $ start "tom.torrent" 
          print result
        


start :: String -> ExceptT String IO [(N.HostName, N.PortNumber)]
start tracker = 
     do ipsPorts <-  CN.makePeers tracker
        let ipsAndPorts = (\(s, ip)-> (BC.pack s, fromIntegral ip)) <$> ipsPorts
        info <- CN.getInfoHash tracker
        liftIO $ print $ "Ips and Ports " ++ (show ipsAndPorts)
        liftIO $ runClient (ipsAndPorts !! 1) info 
        return ipsPorts
        
 
 
runClient :: (B.ByteString, Int) -> B.ByteString -> IO ()
runClient ipAndPort infoHash = 
    CN.runTCPClient ((uncurry CN.clientSettings) . swap $ ipAndPort) $ \appData -> do
      --  print "RUNCLIENT"
        T.sendHandshake infoHash appData
        T.tube appData
   --     runResourceT $ CN.appSource appData $$ getMessage
        
                             

          
                         