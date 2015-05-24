{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, FlexibleInstances, UndecidableInstances, FlexibleContexts, TupleSections #-}

module PeersControler (start) where

import qualified Connector as C (liftEither, makePeers) 
import qualified Peer as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BC
import qualified Message as M
import qualified Crypto.Hash.SHA1 as SHA1 (hash)
import Control.Concurrent.Async as Async (mapConcurrently)
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
                              p@(M.Piece (i,b,c)) -> (print $ "got "++(show b))>>piece peer p
                              _-> loopAndWait peer (show msg) 10000
                     else print "Cant talk !!!!!!!!!!!!!!!!!!!!!!!!!!!"
                     where 
                        loopAndWait peer m p=  (threadDelay p) >> (print m)>>talkToPeer peer

 
reqNextPice :: P.Peer -> IO ()
reqNextPice peer = do nextLs <- P.nextPiceToRequest peer
                      case nextLs of
                           []        -> print "Finisched"
                           (x, b):xs -> (print "req")>>(M.sendMsg peer $ M.Request (22, 0* 16384, 16384))
                                            >> print ("Request "++ (show x))
                                            >> talkToPeer peer
                   
                   

--piece :: P.Peer -> IO ()
piece peer (M.Piece (i,b,c)) = do P.appendToBuffer peer c
                                  if (next>= 32*16384)
                                  then do bf <- readIORef (P.buffer peer)
                                          print $ (SHA1.hash (P.getBuffer2BS bf)) == (Seq.index (P.hashes peer) 22 )--(P.appendBuffToFile peer (show i)) -- >>reqNextPice peer
                                  else (print ("Request subPice "++ (show next)))>>(M.sendMsg peer $ M.Request (i, next, 16384))
                                            >> talkToPeer peer
                               where next = b+16384
                              
                   
                   
bitfield :: P.Peer -> M.MsgPayload -> IO()
bitfield peer bf = P.updateBF peer bf
                   >> sendInterested peer
                   >> print "Got BF" >> (talkToPeer peer)


have :: P.Peer->Int ->IO()
have peer pId = P.updateBFIndex peer pId    -- Interested?
                >> sendInterested peer
                >> print "have" >> (talkToPeer peer)
 
sendInterested peer =  (M.sendMsg peer M.Interested)
 
  