
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module PeersControlerN where

import qualified Connector as CN (liftEither, makePeers, getInfoHash)
import Control.Monad.Except (ExceptT, liftIO, runExceptT)
import qualified Peer as P
import qualified StaticQueue as SQ
import qualified Data.Conduit.Network as CN
import Data.Conduit
import qualified Data.ByteString.Char8 as BC
import qualified Data.Conduit.Binary as CB
import Control.Monad.Trans.Resource

import qualified Tube as T



main::IO() 
main = do result <- runExceptT $ start   "tom.torrent"--"ubuntu.torrent"  -- "tom.torrent"--
          print result

          
          
start :: String -> ExceptT String IO ()
start tracker = 
     do peers  <-  CN.makePeers tracker
        let peer = peers !! 1
        liftIO $ print peers   
        liftIO $ runClient peer
        return ()
        
 
 
runClient :: P.Peer -> IO ()
runClient peer = 
    CN.runTCPClient (CN.clientSettings (P.port peer) (BC.pack $ P.hostName peer)) $ \appData -> do
        let source = CN.appSource appData   
            peerSink   = CN.appSink appData     
   
        T.tube peer source peerSink saveToFile
        
                             
saveToFile :: Sink (String, BC.ByteString) IO ()
saveToFile = do
  awaitForever (liftIO . save)
  where 
    save :: (String, BC.ByteString) -> IO()
    save (fN, c) =
       runResourceT $ 
        (yield c) 
        $$ (CB.sinkFile ("downloads/" ++ fN))
          
                         