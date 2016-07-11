
module  Tracker.UDPTracker where

import qualified Data.Conduit.Network.UDP as UDPC
import qualified Network.Socket as N
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Conduit
import qualified Data.ByteString.Char8 as C8
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Types
import Control.Monad.Trans.Except

--import Control.Concurrent
import qualified Bencode.BInfo as BI

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
               }


data AnnounceRsp =
    AnnounceRsp { actR     :: Int32
                , tranR_id :: Int32
                , interval :: Int32
                , leechers :: Int32
                , seeders  :: Int32
                , hips     :: [(N.HostName, N.PortNumber)]
                } deriving Show


announceRspGet :: Get AnnounceRsp
announceRspGet = do
    actR      <- getInt32be
    tranR     <- getInt32be
    intervalR <- getInt32be
    leechersR <- getInt32be
    seedersR  <- getInt32be
    handIps   <- get32and16b
    return $ AnnounceRsp actR tranR intervalR leechersR seedersR handIps
    where
        get32and16b :: Get [(N.HostName, N.PortNumber)]
        get32and16b =
            do empty <- isEmpty
               if empty then
                   return []
               else do
                   ip <- show <$> getWord32be
                   port <- fromIntegral <$> getWord16be
                   rest <- get32and16b
                   return $ (ip, port):rest



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
       N.connect sock (N.addrAddress serveraddr)
       return sock


-- =====
connectMsg = encode $ ConnectMsg 4497486125440 0 99


trackerResp :: Sink UDPC.Message IO (Either String ConnectMsg)
trackerResp =
    await >>= maybe (return $ Left "No ConnectMsg RSP")
                    (return . Right . convertToConnect)
    where
        convertToConnect s =
            decode $ BL.fromChunks [UDPC.msgData s]


sinkAnnResp ::  Sink UDPC.Message IO (Either String AnnounceRsp)
sinkAnnResp =
    do mx <- await
       case mx of
           Nothing  -> return $ Left "No ConnectMsg RSP"
           Just m ->
               case (convertToAnn m) of
                   Left (bs, bo, s) -> return $ Left s
                   Right (bs, bo, x) -> return $ Right x

    where
        convertToAnn m =
            runGetOrFail announceRspGet (BL.fromChunks [(UDPC.msgData m)])

-- TODO close socket
getHostsAndIps :: N.HostName
               -> String
               -> B.ByteString
               -> ExceptT String IO [(N.HostName, N.PortNumber)]--AnnounceRsp
getHostsAndIps udp port infoHash = do
    socket <- liftIO $ getSocket udp port

    let sinkSocket   = UDPC.sinkSocket socket
        sourceSocket = UDPC.sourceSocket socket 10000

    liftIO $ sendConnect sinkSocket

    (ConnectMsg conn_id act trans_id) <- getRespFromTracker sourceSocket

    let announceMsg = mkAnnounce conn_id trans_id infoHash

    liftIO $ sendAnnounceMsg announceMsg sinkSocket

    aRsp <- ExceptT $ sourceSocket $$ sinkAnnResp

    return $ hips aRsp
--    return ()
    where
        sendConnect sinkSocket =
             (yield $ BL.toStrict connectMsg) $$ sinkSocket

        getRespFromTracker sourceSocket =
             ExceptT $ sourceSocket $$ trackerResp

        sendAnnounceMsg msg sinkSocket =
            let enc =  BL.toStrict $ runPut $ announcePut msg
            in  (yield enc) $$ sinkSocket



torrent = "/Users/blaze/Projects/Haskell/horrent/app/MOS.torrent"


sinkConn :: Sink UDPC.Message IO ConnectMsg
sinkConn = do
    x <- await
    case x of
        Just m -> do
            return (decode $ BL.fromChunks [(UDPC.msgData m)])


{--

sinkAnn ::  Sink UDPC.Message IO ()
sinkAnn = do
    x <- await
    case x of
        Just m -> liftIO $
          print $ runGetOrFail announceRspGet (BL.fromChunks [(UDPC.msgData m)])

--}


getInfo :: IO String
getInfo = do
    dict <- runExceptT $ BI.parseFromFile torrent
    case dict of
        Left l -> error "booo"
        Right x -> return $ case (BI.infoHash x) of
                            Left ll -> error "boo2"
                            Right c -> c



sendAnnounce ann sink = (yield ann) $$ sink

xxx = do
    let udp = "tracker.opentrackr.org" --""/announce"
        port = "1337"

    info <- liftIO $ C8.pack <$> getInfo

    rsp <- getHostsAndIps udp port info

    return $ rsp

main = runExceptT xxx

    {--
        socket <- getSocket udp port

        let sinkI = UDPC.sinkSocket socket :: Consumer B.ByteString IO ()
            sourceI = UDPC.sourceSocket socket 10000 :: Producer IO UDPC.Message

        (yield $ BL.toStrict connectMsg) $$ sinkI


        print "ls"

        (ConnectMsg conn_id act trans_id) <- sourceI $$ sinkConn

        print "ls2"
    --}
    --info <- liftIO $ C8.pack <$> getInfo


{--
    let announceMsg = mkAnnounce conn_id trans_id info
        enc =  BL.toStrict $ runPut $ announcePut announceMsg ::  B.ByteString

    (yield enc) $$ sinkI

    sourceI $$ sinkAnn

    print (conn_id, act, trans_id) --}
