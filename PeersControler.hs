{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, FlexibleInstances, UndecidableInstances, FlexibleContexts, TupleSections #-}

module PeersControler (start) where

import qualified Connector as C (liftEither, makePeers) 
import qualified Peer as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BC
import qualified Message as M
import Control.Concurrent.Async as Async (mapConcurrently)
import qualified Crypto.Hash.SHA1 as SHA1 (hash)
import qualified Data.Sequence as Seq
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Error
import Control.Concurrent.STM
import Data.Array.MArray
import Data.IORef
  
                       
                          
                          
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
                        msg <-  M.getMessage handle--  liftM lenAndIdToMsg lenAndId
                        case msg of
                              M.KeepAlive     -> loopAndWait peer "Alive" 100000
                              M.UnChoke       -> reqNextPice peer 
                              M.Bitfield bf -> bitfield peer bf
                              M.Have (pId, b) ->  have peer pId
                              p@(M.Piece (i,b,c)) -> (print $ "got "++(show i)++" "++(show b))>>piece peer p
                              _-> loopAndWait peer (show msg) 10000
                     else print "Cant talk !!!!!!!!!!!!!!!!!!!!!!!!!!!"
                     where 
                        loopAndWait peer m p=  (threadDelay p) >> (print m)>>talkToPeer peer

 
reqNextPice :: P.Peer -> IO ()  -- TODO clear buffer
reqNextPice peer = do nextLs <- P.nextPiceToRequest peer
                      case nextLs of
                           []        -> print "Finisched"
                           (x, b):xs -> (print "req")>>(M.sendMsg peer $ M.Request (x, 0* 16384, 16384))
                                            >> print ("Request "++ (show x))
                                            >> talkToPeer peer
                   
                   

--piece :: P.Peer -> IO ()
piece peer (M.Piece (i,b,c)) = do if b == 0 then 
                                     P.updateStatusPending i peer
                                     else return ()
                                  P.appendToBuffer peer c
                                  if (next>= 32*16384)
                                  then do bufferS <- readIORef $ P.buffer peer
                                          let buff = P.getBuffer2BS bufferS
                                          let correct = checkHashesEq peer buff i 
                                          if correct 
                                             then B.writeFile ("downloads/"++show i) buff>> P.clearBuffer peer >> P.updateStatusDone i peer >> reqNextPice peer
                                             else print "Wrong Hash" >> (P.resetStatus i peer) >> P.clearBuffer peer 
                                  else (M.sendMsg peer $ M.Request (i, next, 16384))>> talkToPeer peer
                               where next = b+16384
                                     checkHashesEq peer buff idx = 
                                       SHA1.hash buff == Seq.index (P.hashes peer) idx                              
                              
                   
bitfield :: P.Peer -> M.MsgPayload -> IO()
bitfield peer bf = P.updateBF peer bf
                   >> sendInterested peer
                   >> print "Got BF" >> (talkToPeer peer)


have :: P.Peer->Int ->IO()
have peer pId = P.updateBFIndex peer pId    -- Interested?
                >> sendInterested peer
                >> print "have" >> (talkToPeer peer)
 
sendInterested peer =  (M.sendMsg peer M.Interested)
 
  