
module  UDPTracker where

import qualified Data.Conduit.Network.UDP as UDPC
import qualified Network.Socket as N
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int


data ConnectMsg =
    ConnectMsg { connection_id:: Int64 -- 4497486125440
               , action :: Int32
               , transaction_id :: Int32
               }


instance Binary ConnectMsg where

    put msg = putInt64be (connection_id msg)
            >> putInt32be (action msg)
            >> putInt32be (transaction_id msg)

    get = do
        action <- getInt32be
        transaction <- getInt32be
        connection <- getInt64be
        return $ ConnectMsg connection action transaction




getSocket :: N.HostName -> String -> IO N.Socket
getSocket hostName port =
    do addrinfos <- N.getAddrInfo Nothing (Just hostName) (Just port)
       let serveraddr = head addrinfos
       sock <- N.socket (N.addrFamily serveraddr) N.Datagram N.defaultProtocol
       return $  sock


source :: C.Source IO B.ByteString
source = undefined

connect :: N.Socket -> IO ()
connect socket = do
    let sink = UDPC.sinkSocket socket :: C.Consumer B.ByteString IO ()
    source C.$$ sink
    return ()
