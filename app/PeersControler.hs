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
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Concurrent.STM
import Data.Array.MArray
import Data.IORef
  
--"Request 9  -DE1360-RUby~KI.D3!3 16384"
-- "DOWNLOADED  9   -DE1360-RUby~KI.D3!3"                       
                          
  
  
  
  
  
  
start :: String -> Int -> ExceptT String IO [P.Peer]                     
start tracker n= do peers <- C.makePeers tracker n 
                    liftIO $ print peers
                    liftIO $ Async.mapConcurrently talk peers
                    return peers
      

talk :: P.Peer -> IO ()
talk peer =  E.catch (talkToPeer peer) (\(e::E.SomeException) ->{-- P. resetStatus peer)>>--}print $ "Failure "++(P.peerP peer) ++(show e) {-- TODO close the connection-} )

                            
talkToPeer :: P.Peer -> IO ()
talkToPeer peer = do --canTalk <- P.canTalkToPeer peer 
                     if (True) then do
                        let handle = P.handleP peer
                        msg <-  M.getMessage handle--  liftM lenAndIdToMsg lenAndId
                        
                        
                        case msg of
                              M.KeepAlive     -> loopAndWait peer "Alive" 100000
                              M.UnChoke       -> reqNextPice peer 
                              M.Bitfield bf -> bitfield peer bf
                              M.Have (pId, b) ->  have peer pId
                              p@(M.Piece (i,b,c)) -> piece peer p
                              _-> loopAndWait peer (show msg) 10000
                     else print "Cant talk !!!!!!!!!!!!!!!!!!!!!!!!!!!"
                     where 
                        loopAndWait peer m p=  (threadDelay p) >> (print m)>>talkToPeer peer

 
reqNextPice :: P.Peer -> IO ()  -- TODO clear buffer
reqNextPice peer = do nextLs <- P.nextPiceToRequest peer  -- TODO change status to Req, return MAYBE
                      case nextLs of
                           []        -> print "Finisched"
                           (x, b):xs -> (M.sendMsg peer $ M.Request (x, 0, maxS))
                                        >> print ("Request "++ (show x)++"  "++ (P.peerP peer)++" "++(show maxS))
                                        >> talkToPeer peer
                                        where maxS = maxSize peer x
                   
maxSize peer i = if (i < (nbOfPeers-1)  ) then
                    maxSize
                    else lastSize
  where (nbOfPeers, maxSize, lastSize) = P.sizeInfo peer
                   
  

--piece :: P.Peer -> IO ()
piece peer (M.Piece (i,b,c)) = do if b == 0 then 
                                     P.updateStatusPending i peer
                                     else return ()
                                  P.appendToBuffer peer c
                                  if (next >= maxS) 
                                     then do bufferS <- readIORef $ P.buffer peer
                                             let buff = P.getBuffer2BS bufferS
                                             let correct = checkHashesEq peer buff i 
                                             if correct 
                                                then do B.writeFile ("downloads/"++show i) buff
                                                        print ("DOWNLOADED  " ++show i ++"   "++ (P.peerP peer))
                                                        P.clearBuffer peer
                                                        P.updateStatusDone i peer
                                                        reqNextPice peer 
                                                else print "HASH NOT CORRECT"
                                    else (M.sendMsg peer $ M.Request (i, next, maxS)) >> (talkToPeer peer)  
                               where    
                                     maxS = maxSize peer i
                                     next = b+maxS
                                     checkHashesEq peer buff idx = 
                                                   SHA1.hash buff == Seq.index (P.hashes peer) idx                              
                              
                   
bitfield :: P.Peer -> M.MsgPayload -> IO()
bitfield peer bf = P.updateBF peer bf
                   >> sendInterested peer
                   >> print ("Got BF  "++ (show bf)) >> (talkToPeer peer)


have :: P.Peer->Int ->IO()
have peer pId = P.updateBFIndex peer pId    -- Interested?
                >> sendInterested peer
                >> print "have" >> (talkToPeer peer)
 
sendInterested peer =  (M.sendMsg peer M.Interested)
 
  