
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module PeersControlerN where

import qualified Connector as CN (liftEither, makePeers)
import Control.Monad.Except (ExceptT, liftIO, runExceptT)
import qualified Peer as P
import qualified Network as N
import qualified StaticQueue as SQ
import qualified Data.Conduit.Network as CN
import Data.Tuple (swap)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC



main::IO() 
main = do result <- runExceptT $ start "tom.torrent" 
          print result
        


start :: String -> ExceptT String IO [(N.HostName, N.PortNumber)]
start tracker = 
     do ipsPorts <-  CN.makePeers tracker
        let ipsAndPorts = (\(s, ip)-> (BC.pack s, fromIntegral ip)) <$> ipsPorts
        liftIO $ runClient $ head ipsAndPorts
        return ipsPorts
        
 
 
runClient :: (B.ByteString, Int) -> IO ()
runClient ipAndPort = 
    CN.runTCPClient ((uncurry CN.clientSettings) . swap $ ipAndPort) $ \server ->
        undefined 