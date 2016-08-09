{-# LANGUAGE NoMonomorphismRestriction, ConstraintKinds, ScopedTypeVariables, FlexibleContexts #-}

module  Tracker.UDPTracker where

import qualified Data.Conduit.Network.UDP as UDPC
import qualified Network.Socket as N
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Bencode.BInfo as BI
import qualified Types as TP

import Tracker.TrackerUtils (get32and16b)

import Data.Conduit
import Data.Binary (Binary, Word32, Word16, get, put, decode, encode)
import qualified Data.Binary.Get as Get--(Get, getInt64be, getInt32be, runGetOrFail)
import qualified Data.Binary.Put as Put
import Data.Int (Int32, Int64)
import Control.Monad.Trans.Except
import Control.Exception as E
import Utils (io2ExceptT)
import Control.Monad (join)

import Control.Monad.IO.Class
import Control.Monad.Except
import Logger.BasicLogger

myId = BC.pack "-TR2840-d0p22uiake0b"


data AnnounceMsg =
    AnnounceMsg { aMconnectionId  :: Int64
                , aMaction        :: Int32
                , aMtransactionId :: Int32
                , aMinfoHash      :: B.ByteString  -- 20
                , aMpeerId        :: B.ByteString  -- 20
                , aMdownloaded    :: Int64
                , aMleft          :: Int64
                , aMuploaded      :: Int64
                , aMevent         :: Int32
                , aMip            :: Word32
                , aMkey           :: Word32
                , aMnumWant       :: Int32
                , aMport          :: Word16
                }


announcePut :: AnnounceMsg -> Put.Put
announcePut msg =
    Put.putInt64be (aMconnectionId msg)
    >> Put.putInt32be (aMaction msg)
    >> Put.putInt32be (aMtransactionId msg)
    >> Put.putByteString (aMinfoHash msg)
    >> Put.putByteString (aMpeerId msg)
    >> Put.putInt64be (aMdownloaded msg)
    >> Put.putInt64be (aMleft msg)
    >> Put.putInt64be (aMuploaded msg)
    >> Put.putInt32be (aMevent msg)
    >> Put.putWord32be (aMip msg)
    >> Put.putWord32be (aMkey msg)
    >> Put.putInt32be (aMnumWant msg)
    >> Put.putWord16be (aMport msg)


mkAnnounce conn_id trans_id info =
    AnnounceMsg{ aMconnectionId  = conn_id
               , aMaction        = 1
               , aMtransactionId = trans_id
               , aMinfoHash      = info
               , aMpeerId        = myId
               , aMdownloaded    = 0
               , aMleft          = 1000000
               , aMuploaded      = 0
               , aMevent         = 2
               , aMip            = 0
               , aMkey           = 998 -- this must be random
               , aMnumWant       = -1
               , aMport          = 6881 -- ???
               }


data AnnounceRsp =
    AnnounceRsp { aRactionId      :: Int32
                , aRtransactionId :: Int32
                , aRinterval      :: Int32
                , aRleechers      :: Int32
                , aRseeders       :: Int32
                , aRhips          :: [(N.HostName, N.PortNumber)]
                } deriving Show


announceRspGet :: Get.Get AnnounceRsp
announceRspGet = do
    actR      <- Get.getInt32be
    tranR     <- Get.getInt32be
    intervalR <- Get.getInt32be
    leechersR <- Get.getInt32be
    seedersR  <- Get.getInt32be
    handIps   <- get32and16b
    return $ AnnounceRsp actR tranR intervalR leechersR seedersR handIps



data ConnectMsg =
    ConnectMsg { cMconnectionId  :: Int64 -- 4497486125440
               , cMaction        :: Int32
               , cMtransactionId :: Int32
               } deriving Show


instance Binary ConnectMsg where

    put msg =
        Put.putInt64be (cMconnectionId msg)
        >> Put.putInt32be (cMaction msg)
        >> Put.putInt32be (cMtransactionId msg)

    get = do
        action      <- Get.getInt32be
        transaction <- Get.getInt32be
        connection  <- Get.getInt64be
        return $ ConnectMsg connection action transaction



getSocket :: N.HostName -> String ->  IO N.Socket
getSocket hostName port = do
    addrinfos <- N.getAddrInfo Nothing (Just hostName) (Just port)
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
    await >>= maybe (return $ Left "No ConnectMsg RSP")
                    (return . mapConvert . convertToAnn)

    where
        convertToAnn m =
            Get.runGetOrFail announceRspGet (BL.fromChunks [UDPC.msgData m])

        mapConvert (Left (bs, bo, s))  = Left s
        mapConvert (Right (bs, bo, x)) = Right x



-- TODO close socket
getHostsAndIps :: ( MonadLogger m l
                  , MonadIO m
                  , MonadError String m)
               =>B.ByteString
               -> String
               -> m [(N.HostName, N.PortNumber)]
getHostsAndIps tracker infoHash = do
    (hn, port) <-  TP.tryEither $ BI.parseUDPAnnounce tracker

    socket <- liftIO $ getSocket (BC.unpack hn) (BC.unpack port)

    let sinkSocket   = UDPC.sinkSocket socket
        sourceSocket = UDPC.sourceSocket socket 10000

    liftIO $ sendConnect sinkSocket

-- wow!!! wtf!
    kk <- liftIO $ getRespFromTracker sourceSocket

    (ConnectMsg conn_id act trans_id) <- TP.tryEither kk

    let announceMsg = mkAnnounce conn_id trans_id (BC.pack infoHash)

    liftIO $ sendAnnounceMsg announceMsg sinkSocket

    aRsp <- liftIO $ sourceSocket $$ sinkAnnResp

    liftIO $ N.sClose socket
    TP.tryEither $ fmap aRhips aRsp

    where
        sendConnect sinkSocket =
             yield (BL.toStrict connectMsg) $$ sinkSocket

        getRespFromTracker :: Source IO UDPC.Message -> IO (Either String ConnectMsg)
        getRespFromTracker sourceSocket =
               sourceSocket $$ trackerResp

        sendAnnounceMsg msg sinkSocket =
            let enc =  BL.toStrict $ Put.runPut $ announcePut msg
            in  yield enc $$ sinkSocket
