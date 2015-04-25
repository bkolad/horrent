{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, FlexibleInstances, UndecidableInstances, FlexibleContexts, TupleSections #-}

module PeersControler (start) where

import qualified Connector as C (liftEither, makePeers) 
import qualified Peer as P
import Control.Concurrent.Async as Async (mapConcurrently)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BC
import qualified System.IO as SIO
import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Error

type MsgLen = Word32

type MsgPayload = B.ByteString

data Message = KeepAlive 
             | Choke
             | UnChoke
             | Interested
             | NotInterested
             | Have (Int, B.ByteString)
             | Bitfield MsgPayload
             | Request
             | Piece
             | Cancel
             | Port 
             | Unknown MsgLen MessageId
             deriving (Show)
             
data MessageId = NoId | Id Int deriving (Show)     

msgToLenAndId :: Message -> (MsgLen, MessageId)
msgToLenAndId msg = case msg of
                         KeepAlive     -> (0, NoId)
                         Choke         -> (1, Id 0)
                         UnChoke       -> (1, Id 1)
                         Interested    -> (1, Id 2)
                         NotInterested -> (1, Id 3)
                         Have _        -> (5, Id 4)
                         Bitfield _    -> undefined
                         Request       -> (13, Id 6)
                         Piece         -> undefined
                         Cancel        -> (13, Id 8)
                         Port          -> (3, Id 9)
                         Unknown _ _     -> undefined --(99, Id 99)

lenAndIdToMsg :: (MsgLen, MessageId, MsgPayload) -> Message 
lenAndIdToMsg lenId = case lenId of
                          (0, NoId, _)   -> KeepAlive
                          (1, Id 0, bs)   -> Choke
                          (1, Id 1, bs)   -> UnChoke
                          (1, Id 2, bs)   -> Interested
                          (1, Id 3, bs)   -> NotInterested
                          (5, Id 4, pId)  -> Have (P.fromBsToInt pId, pId)
                          (len, Id 5, bf) -> Bitfield bf
                          (13, Id 6, bs)  -> Request
                          (len, Id 7, bs) -> Piece 
                          (13, Id 8, bs)  -> Cancel
                          (3, Id 9, bs)   -> Port
                          (i, iD, _)      -> Unknown i iD                                   

                          
start :: String -> Int -> ErrorT String IO [P.Peer]                     
start tracker n= do peers <- C.makePeers tracker n 
                    liftIO $ Async.mapConcurrently talk peers
                    return peers
      

talk :: P.Peer -> IO ()
talk peer =  E.catch (talkToPeer peer) (\(e::E.SomeException) -> print $ "Failure "++(P.peerP peer) ++(show e) {-- TODO close the connection-} )

                            
talkToPeer :: P.Peer -> IO ()
talkToPeer peer = do canTalk <- P.canTalkToPeer peer 
                     if (canTalk) then do
                        let handle = P.handleP peer
                        lenAndId <- getMessage handle
                        let msg = liftM lenAndIdToMsg lenAndId
                        case msg of
                              Just KeepAlive     -> loopAndWait peer "Alive" 100000
                              Just UnChoke       -> unchoke peer
                              Just (Bitfield bf) -> bitfield peer bf
                              Just (Have (pId, b)) ->  have peer pId
                              Nothing -> print "Nothing"
                              _-> loopAndWait peer (show msg) 10000
                     else print "Cant talk !!!!!!!!!!!!!!!!!!!!!!!!!!!"
                     where 
                        loopAndWait peer m p=  (threadDelay p) >> (print m)>>talkToPeer peer

 
unchoke :: P.Peer -> IO ()
unchoke peer = P.setNotVirgin peer>> (print "UNCHOKED")  -- check if interested and request if yes

bitfield :: P.Peer->MsgPayload->IO()
bitfield peer bf = P.updateBF peer bf 
                   >> P.setInterested peer
                   >> (sendMsg (P.handleP peer) Interested)
                   >> print "Got BF" >> (talkToPeer peer)
                   
have :: P.Peer->Int ->IO()
have peer pId = P.updateBFIndex peer pId    -- Interested?
                >> print "have" >> (talkToPeer peer)
 
 
getMessage :: SIO.Handle -> IO (Maybe (MsgLen, MessageId, MsgPayload))
getMessage handle = do numBytes <- BL.hGet handle 4
                       if (BL.length numBytes) < 4 
                          then return Nothing  
                       else 
                            Just <$> (getMsg (readBEWord numBytes))
                     where
                      getMsg 0 = return (0, NoId, B.empty)
                      getMsg size = (\iD p->(size, iD, p))<$>(Id . P.fromBsToInt <$> B.hGet handle 1)
                                                          <*> (B.hGet handle ((fromIntegral size) -1))
                      readBEWord = runGet getWord32be   
                                                         
                  
sendMsg :: SIO.Handle -> Message -> IO ()
sendMsg handle msg = case (msgToLenAndId msg) of
                          (x, NoId) -> send $ putWord32be x
                          (x, Id y) -> send $ putWord32be x >> putWord8 (fromIntegral y)
                          where 
                            send = BL.hPutStr handle . runPut  
   