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
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Error
import Control.Concurrent.STM
import Data.Array.MArray
import Data.IORef

type MsgLen = Word32

type MsgPayload = B.ByteString


data Message = KeepAlive 
             | Choke
             | UnChoke
             | Interested
             | NotInterested
             | Have (Int, B.ByteString)
             | Bitfield MsgPayload
             | Request (Int, Int, Int)
             | Piece (Int, Int, B.ByteString)
             | Cancel
             | Port 
             | Unknown MsgLen Int
             deriving (Show)
             
                            
  
                       
                          
                          
start :: String -> Int -> ErrorT String IO [P.Peer]                     
start tracker n= do peers <- C.makePeers tracker n 
                    liftIO $ Async.mapConcurrently talk peers
                    return peers
      

talk :: P.Peer -> IO ()
talk peer =  E.catch (talkToPeer peer) (\(e::E.SomeException) -> print $ "Failure "++(P.peerP peer) ++(show e) {-- TODO close the connection-} )

                            
talkToPeer :: P.Peer -> IO ()
talkToPeer peer = do --canTalk <- P.canTalkToPeer peer 
                     if (True) then do
                        let handle = P.handleP peer
                        print "Waiting"
                        msg <-  getMessage handle--  liftM lenAndIdToMsg lenAndId
                        case msg of
                              KeepAlive     -> loopAndWait peer "Alive" 100000
                              UnChoke       -> reqNextPice peer
                              Bitfield bf -> bitfield peer bf
                              Have (pId, b) ->  have peer pId
                              p@(Piece (i,b,c)) -> (print $ "got "++(show b))>>piece peer p
                              _-> loopAndWait peer (show msg) 10000
                     else print "Cant talk !!!!!!!!!!!!!!!!!!!!!!!!!!!"
                     where 
                        loopAndWait peer m p=  (threadDelay p) >> (print m)>>talkToPeer peer

 
reqNextPice :: P.Peer -> IO ()
reqNextPice peer = do nextLs <- P.nextPiceToRequest peer
                      case nextLs of
                           []        -> print "Finisched"
                           (x, b):xs -> (print "req")>>(sendMsg peer $ Request (10, 0* 16384, 16384))
                                            >> print ("Request "++ (show x))
                                            >> talkToPeer peer
                   
                   

--piece :: P.Peer -> IO ()
piece peer (Piece (i,b,c)) = do P.appendToBuffer peer c
                                if (next>= 32*16384)
                                then (P.appendBuffToFile peer (show i)) -- >>reqNextPice peer
                                else (print ("Request subPice "++ (show next)))>>(sendMsg peer $ Request (i, next, 16384))
                                           >> talkToPeer peer
                              where next = b+16384
                              
                   
                   
bitfield :: P.Peer -> MsgPayload -> IO()
bitfield peer bf = P.updateBF peer bf
                   >> sendInterested peer
                   >> print "Got BF" >> (talkToPeer peer)


have :: P.Peer->Int ->IO()
have peer pId = P.updateBFIndex peer pId    -- Interested?
                >> sendInterested peer
                >> print "have" >> (talkToPeer peer)
 
sendInterested peer =  (sendMsg peer Interested)
 
   
instance Binary Message where
  
   put KeepAlive           = putWord32be 0
   put Choke               = putWord32be 1 >> putWord8 0
   put UnChoke             = putWord32be 1 >> putWord8 1
   put Interested          = putWord32be 1 >> putWord8 2
   put NotInterested       = putWord32be 1 >> putWord8 3
   put (Request (i, o, l)) = putWord32be 13 >> putWord8 6 >> put32Int i 
                                                          >> put32Int o 
                                                          >> put32Int l
                                 
       where  put32Int = putWord32be . fromIntegral    
   
   
   put (Piece _)           = undefined
   put Cancel              = putWord32be 13 >> putWord8 8
   put Port                = putWord32be 3 >> putWord8 9
   put (Unknown _ _)       = undefined 
   
   
   
   get = do numBytes <- fromIntegral <$> getWord32be
            if (numBytes == 0)
               then return KeepAlive
               else do idx <- getWord8
                       let size = numBytes -1  
                       bs <- getByteString size
                       case idx of
                             0 -> return Choke
                             1 -> return UnChoke
                             2 -> return Interested
                             3 -> return NotInterested
                             4 -> return $ Have (P.fromBsToInt bs, bs)
                             5 -> return $ Bitfield bs
                             6 -> return $ Request (undefined, undefined, undefined)
                             7 -> return $ Piece (toPiece bs)
                             8 -> return $ Cancel
                             9 -> return $ Port
                             x -> return $ Unknown (fromIntegral size) (fromIntegral x)
       
       
toPiece bs = runGet getTripplet (BL.fromChunks [bs])
  where getTripplet :: Get (Int, Int, B.ByteString)
        getTripplet = do idx <-  getWord32be
                         begin <- getWord32be
                         rest <- getRemainingLazyByteString
                         return $ (fromIntegral idx, fromIntegral begin, (B.concat . BL.toChunks) rest)
   
   
getMessage :: SIO.Handle -> IO Message 
getMessage handle =  decode <$> (BL.hGetContents handle)
                                                         
                  
sendMsg :: P.Peer -> Message -> IO ()
sendMsg peer msg = send msg
  where handle = P.handleP peer
        send = BL.hPutStr handle . encode  
                          
                 