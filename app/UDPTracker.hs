
module  UDPTracker where

import qualified Data.Conduit.Network.UDP as UDPC
import qualified Network.Socket as N
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Conduit as C
import qualified Data.ByteString.Char8 as C8
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Types
import Control.Concurrent
import qualified BencodeInfo as BI

myId = C8.pack "-TR2840-d0p22uiake0b"


data AnnounceMsg =
    AnnounceMsg { connection_idA  :: Int64
                , actionA         :: Int32
                , transaction_idA :: Int32
                , infoHash        :: B.ByteString  -- 20
                , peerId          :: B.ByteString  -- 20
                , downloaded      :: Int64
                , left            :: Int64
                , uploaded        :: Int64
                , event           :: Int32
                , ip              :: Word32
                , key             :: Word32
                , num_want        :: Int32
                , port            :: Word16
        --        , extensions      :: Word32 -- ?? not in officail info
                }


announcePut :: AnnounceMsg -> Put
announcePut msg = putInt64be (connection_idA msg)
                >> putInt32be (actionA msg)
                >> putInt32be (transaction_idA msg)
                >> putByteString (infoHash msg)
                >> putByteString (peerId msg)
                >> putInt64be (downloaded msg)
                >> putInt64be (left msg)
                >> putInt64be (uploaded msg)
                >> putInt32be (event msg)
                >> putWord32be (ip msg)
                >> putWord32be (key msg)
                >> putInt32be (num_want msg)
                >> putWord16be (port msg)
        --        >> putWord32be (extensions msg) -- ??


--mkAnnounce :: AnnounceMsg
mkAnnounce conn_id trans_id info =
    AnnounceMsg{ connection_idA  = conn_id
               , actionA         = 1
               , transaction_idA = trans_id
               , infoHash        = info
               , peerId          = myId
               , downloaded      = 0
               , left            = 1000000
               , uploaded        = 0
               , event           = 2
               , ip              = 0
               , key             = 998 -- this must be random
               , num_want        = -1
               , port            = 6881 -- ???
    --           , extensions      :: Word32 -- ?? not in officail info
               }



data AnnounceRsp =
    AnnounceRsp { actR     :: Int32
                , tranR_id :: Int32
                , interval :: Int32
                , leechers :: Int32
                , seeders :: Int32
                } deriving Show

announceRspGet :: Get AnnounceRsp
announceRspGet = do
    actR      <- getInt32be
    tranR     <- getInt32be
    intervalR <- getInt32be
    leechersR <- getInt32be
    seedersR  <- getInt32be
    return $ AnnounceRsp actR tranR intervalR leechersR seedersR


data ConnectMsg =
    ConnectMsg { connection_id  :: Int64 -- 4497486125440
               , action         :: Int32
               , transaction_id :: Int32
               } deriving Show


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
       N.connect sock (N.addrAddress serveraddr) -- >> return s
       return sock


sourceConn :: C.Source IO B.ByteString
sourceConn = do
    let msg =  encode $ ConnectMsg 4497486125440 0 99
    C.yield $ BL.toStrict msg


sinkConn :: C.Sink UDPC.Message IO ConnectMsg
sinkConn = do
    x <- C.await
    case x of
        Just m -> do
            return (decode $ BL.fromChunks [(UDPC.msgData m)] :: ConnectMsg)


torrent = "/Users/blaze/Projects/Haskell/horrent/app/MOS.torrent"


sinkAnn ::  C.Sink UDPC.Message IO ()
sinkAnn = do
    x <- C.await
    case x of
        Just m -> liftIO $
          print $ runGetOrFail announceRspGet (BL.fromChunks [(UDPC.msgData m)])

getInfo = do
    dict <- runExceptT $ BI.parseFromFile torrent
    case dict of
        Left l -> error "booo"
        Right x -> return $ case (BI.infoHash x) of
                            Left ll -> error "boo2"
                            Right c -> c



senAnnounce ann sink = (C.yield ann) C.$$ sink

main = do
    let udp = "tracker.opentrackr.org" --""/announce"
        udp2 = "tracker.coppersurfer.tk"
        port2 = "6969"
        port = "1337"

    socket <- getSocket udp port

    let sinkI = UDPC.sinkSocket socket :: C.Consumer B.ByteString IO ()
        sourceI = UDPC.sourceSocket socket 1000 :: C.Producer IO UDPC.Message

    forkIO $ do
        print "ls"

        (ConnectMsg conn_id act trans_id) <- sourceI C.$$ sinkConn

        info <- C8.pack <$> getInfo
        let announceMsg = mkAnnounce conn_id trans_id info
            enc =  BL.toStrict $ runPut $ announcePut announceMsg ::  B.ByteString

        (C.yield enc) C.$$ sinkI

        sourceI C.$$ sinkAnn

        print (conn_id, act, trans_id)

    sourceConn C.$$ sinkI

    --sendConnect socket
    threadDelay 2000000000
    return()
