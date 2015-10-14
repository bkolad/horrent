
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
import qualified Types as TP



main::IO() 
main = do result <- runExceptT $ start  "ubuntu.torrent" -- "tom.torrent"--"ubuntu.torrent"  -- "tom.torrent"--
          print result

          
          
start :: String -> ExceptT String IO ()
start tracker = 
     do peers  <-  CN.makePeers tracker
        let peer = peers !! 2
        liftIO $ print peers   
        liftIO $ runClient peer
        return ()
        
 
 
runClient :: P.Peer -> IO ()
runClient peer = 
    CN.runTCPClient (CN.clientSettings (P.port peer) (BC.pack $ P.hostName peer)) $ \appData -> do
        T.tube peer appData
        
                             

          
                         