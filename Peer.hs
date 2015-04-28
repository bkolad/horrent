{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module Peer (Peer, makePeer, myId, showPeer, peerP, handleP, setInterested, 
setNotVirgin, getBitFieldList, {--canTalkToPeer,--} updateBF, fromBsToInt, 
updateBFIndex, amIInterested, bitFieldArray, nextPiceToRequest) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import qualified Network as N
import qualified System.IO as SIO
import qualified Data.Bits as Bits
import qualified Control.Concurrent.STM.TArray as TA
import Control.Concurrent.STM
import Data.IORef
import Data.List
import Control.Applicative
import Data.Array.MArray
import Types


myId = "-TR2840-d0p22uiake0b" 
protocol = "BitTorrent protocol"


type Bitfield = TA.TArray Int Bool 

data Progress = NextId Int | Finished

data Peer = Peer{ handleP :: SIO.Handle
                , peerP :: String 
                , amIInterested :: TVar Bool -- false
                , amIChocked :: IORef Bool  -- true
                , amIVirgin :: IORef Bool -- first time I am talking to a peer
                , bitFieldArray :: Bitfield
                , globalIndexArray :: GlobalPiceInfo -- TO do diffrent statuste req, pending, done 
                } 
                

newStm = atomically (newTVar False)
                   
  
makeBFArray ::Int-> IO Bitfield  
makeBFArray size = atomically $ newArray (0, size-1) False  

getBitFieldList peer = atomically $ let arr =  bitFieldArray peer
                                    in getAssocs arr

nextPiceToRequest :: Peer -> IO [(Int, Bool)]
nextPiceToRequest peer = atomically $ let global = globalIndexArray peer
                                          bf = bitFieldArray peer
                                      in arrayDiff bf global  
                                                                                   
          
arrayDiff:: (MArray a1 e m, MArray a e m, Applicative m, Ix i, Eq e) => a i e -> a1 i e -> m [(i, e)]
arrayDiff arr1 arr2 = ((\\))<$> (getAssocs arr1) <*> (getAssocs arr2)
                  
                   
setNotVirgin :: Peer -> IO ()                       
setNotVirgin peer = modifyIORef' (amIVirgin peer) (\_->False)                         

setInterested :: Peer-> IO () 
setInterested peer = atomically $ writeTVar (amIInterested peer) True

--setNotInterested :: Peer-> IO () 
--setNotInterested peer = atomically $ writeTVar (amIInterested peer) False 


updateBF :: Peer -> BC.ByteString -> IO ()
updateBF peer bf = atomically $ updateArray (bitFieldArray peer) bf 
  
updateBFIndex :: Peer -> Int -> IO ()
updateBFIndex peer i = atomically $  writeArray (bitFieldArray peer) i True
                                             
                          
                          
                       {--    
canTalkToPeer :: Peer -> IO Bool
canTalkToPeer peer = do isVirgin <- readIORef (amIVirgin peer)    
                        isInterested <-readIORef (amIInterested peer)                     
                        isIChocked <- readIORef (amIChocked peer)
                        return $  isVirgin || (isIChocked && isInterested)               
                        --}
--showPeer :: Peer -> IO String                  
showPeer p= do let name = peerP p
             --  arr <-getElems (bitFieldArray p)    
               return name--(name, arr)
                         
                 
makePeer :: BC.ByteString -> N.HostName -> N.PortNumber -> Int -> GlobalPiceInfo-> IO Peer             
makePeer hash host port size globalPiceInfo = do handle<- N.connectTo host (N.PortNumber  port)
                                                 SIO.hSetBuffering handle SIO.NoBuffering                            
                                                --  SIO.hSetBuffering handle (SIO.BlockBuffering Nothing)
                                                  --hSetBuffering handle LineBuffering
                                                 sendHandshake handle hash $ BC.pack myId
                                                 recvHandshake handle size globalPiceInfo                                      
                                                    
             
sendHandshake :: SIO.Handle -> B.ByteString -> B.ByteString -> IO ()
sendHandshake handle hash peer = BC.hPutStr handle msg -- >> print "Handshake finished"
        where msg = B.concat [len, ptr, rsrv, hash, peer]
              len = B.singleton $ (fromIntegral . length) protocol
              ptr = BC.pack protocol
              rsrv = B.replicate 8 (fromIntegral 0)       
              
              
recvHandshake :: SIO.Handle-> Int -> GlobalPiceInfo -> IO Peer
recvHandshake handle size globalPiceInfo = do len <- B.hGet handle 1
                                              ptr <- B.hGet handle $ fromBsToInt len
                                              rsrv <- B.hGet handle 8
                                              hash <- B.hGet handle 20
                                              peer <- B.hGet handle 20 
                                              amIVirgin <- newIORef True
                                              amIChocked <- newIORef True
                                              amIInterested <- atomically (newTVar False)
                                              bfArr <- makeBFArray size
                                              return $ Peer handle (BC.unpack peer) amIInterested amIChocked amIVirgin bfArr globalPiceInfo

                                  
                    
updateArray:: (MArray a Bool m, Ix i, Num i) =>a i Bool -> BC.ByteString -> m ()
updateArray arr bs = update arr (convertToBits bs) 0   


update :: (MArray a Bool m, Ix i, Num i) => a i Bool -> [Bool] -> i -> m ()
update arr [] _ = return ()
update arr (x:xs) i = do (lo, hi) <- getBounds arr
                         if checkBounds (lo, hi) then
                            (writeArray arr i x) >> update arr xs (i+1)
                         else
                            return ()     
                      where checkBounds (lo,hi) = lo <=i && hi>=i
                        
convertToBits bs = [Bits.testBit w i| w<-B.unpack bs, i<-[7,6.. 0]]
             
fromBsToInt bs = sum $ zipWith (\x y->x*2^y) (reverse ws) [0,8..]
                 where ws = map fromIntegral (B.unpack bs)               
                                                                        



                            
                            
               
