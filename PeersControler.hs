{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}

module PeersControler (start) where

import qualified Connector as C (liftEither, makePeers) 
import qualified Peer as P
import Control.Concurrent.Async as Async (mapConcurrently)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import qualified Data.Bits as Bits
import qualified System.IO as SIO
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.IORef
import Control.Monad.Error
import Data.Array.IO

data Message = KeepAlive 
             | Choke
             | UnChoke
             | Interested
             | NotInterested
             | Have Int
             | Bitfield B.ByteString
             | Request
             | Piece
             | Cancel
             | Port 
             | Error Int MessageId
             deriving (Show)
             
data MessageId = NoId | Id Int deriving (Show)     


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
                         Error _ _     -> undefined --(99, Id 99)

lenAndIdToMsg :: (Int, MessageId, B.ByteString) -> Message 
lenAndIdToMsg lenId = case lenId of
                          (0, NoId, _)   -> KeepAlive
                          (1, Id 0, bs)   -> Choke
                          (1, Id 1, bs)   -> UnChoke
                          (1, Id 2, bs)   -> Interested
                          (1, Id 3, bs)   -> NotInterested
                          (5, Id 4, pId)  -> Have $ P.intFromBS pId
                          (len, Id 5, bf) -> Bitfield bf
                          (13, Id 6, bs)  -> Request
                          (len, Id 7, bs) -> Piece 
                          (13, Id 8, bs)  -> Cancel
                          (3, Id 9, bs)   -> Port
                          (i, iD, _)      -> Error i iD
                        


start :: String -> Int -> ErrorT String IO [P.Peer]                     
start tracker n= do peers <- C.makePeers tracker n 
                    liftIO $ Async.mapConcurrently talk peers
                    return peers
                  

talk :: P.Peer -> IO ()
talk peer =  E.catch (talkToPeer peer) (\(e::E.SomeException) -> print $ "Failure "++(P.peerP peer) ++(show e) {-- TODO close the connection-} )

                            
talkToPeer :: P.Peer -> IO ()
talkToPeer peer = do canTalk <- canTalToPeer peer 
                     if (canTalk) then do
                        let handle = P.handleP peer
                        lenAndId <- getMessage handle
                        let msg = liftM lenAndIdToMsg lenAndId
                        case msg of
                              Just KeepAlive     -> loopAndWait peer "Alive"
                              Just UnChoke       -> modifyIORef' (P.amIVirgin peer) (\_->False) >> (print "UNCHOKED")
                              Just (Bitfield bf) -> modifyIORef' (P.amIInterested peer) (\_->True) 
                                                    >> updateArray  (P.bitFieldArray peer) bf
                                                    >> (sendMsg handle Interested)
                                                    >> (talkToPeer peer)
                          --    Just (Have pId)   ->                      
                              _-> loopAndWait peer (show msg)
                     else print "Cant talk !!!!!!!!!!!!!!!!!!!!!!!!!!!"
                     where 
                        loopAndWait peer m  =  (threadDelay 10000) >> (print m)>>talkToPeer peer

                        
getMessage :: SIO.Handle -> IO (Maybe (Int, MessageId, B.ByteString))
getMessage handle = do numBytes <- BL.hGet handle 4
                       if (BL.length numBytes) < 4 then return Nothing  
                       else 
                          do let sizeOfTheBody = (readBEInt numBytes)
                             case sizeOfTheBody of
                                  0 -> return $ Just (0, NoId, B.empty) 
                                  otherwise -> --(Just . swap)<$>(getMsg handle sizeOfTheBody) 
                                              do 
                                                 (msgId, body) <- getMsg handle sizeOfTheBody 
                                                 return $ Just  (sizeOfTheBody, msgId, body)
                             where  
                               getMsg handle size = (,)<$>(Id . P.intFromBS <$> B.hGet handle 1)<*> (B.hGet handle (size -1))
                               readBEInt = fromIntegral  . runGet getWord32be                              
       
       
canTalToPeer :: P.Peer -> IO Bool
canTalToPeer peer = do isVirgin <- readIORef (P.amIVirgin peer)    
                       isInterested <-readIORef (P.amIInterested peer)                     
                       isIChocked <- readIORef (P.amIChocked peer)
                       return $  isVirgin || (isIChocked && isInterested)
                        
                  
sendMsg :: SIO.Handle -> Message -> IO ()
sendMsg handle msg = case (msgToLenAndId msg) of
                          (x, NoId) -> send $ putWord32be x
                          (x, Id y) -> send $ putWord32be x >> putWord8 (fromIntegral y)
                          where 
                            send = BL.hPutStr handle . runPut  
   
   
updateArray :: IOArray Int Bool -> B.ByteString -> IO ()   
updateArray arr bs = update arr (convert bs) 0   


update :: IOArray Int Bool -> [Bool] -> Int -> IO ()
update arr [] _ = return ()
update arr (x:xs) i = do (lo, hi) <- getBounds arr
                         if (lo>=i && hi<=i) then
                            (writeArray arr i x) >> update arr xs (i+1)
                         else
                            return ()
                          
convert bs = [Bits.testBit w i| w<-B.unpack bs, i<-[7,6.. 0]]
          
          
