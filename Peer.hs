{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module Peer (Peer, makePeer, myId, showPeer, peerP, handleP, setInterested, setNotVirgin, bitFieldArray, canTalkToPeer, updateBF, fromBsToInt, updateBFIndex) where

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
import Data.Array.IO
import Data.List


myId = "-TR2840-d0p22uiake0b" 
protocol = "BitTorrent protocol"


type STMBitfield = STM (TA.TArray Int Bool) 

data Progress = NextId Int | Finished

data Peer = Peer{ handleP :: SIO.Handle
                , peerP :: String 
                , amIInterested :: IORef Bool -- false
                , amIChocked :: IORef Bool  -- true
                , amIVirgin :: IORef Bool -- first time I am talking to a peer
                , bitFieldArray :: STMBitfield
                , globalIndexArray :: STMBitfield
                } 
                

                
newGlobalBitField ::Int-> STMBitfield
newGlobalBitField size= newArray (0, size-1) False     
  
makeBFArray ::Int-> STMBitfield  
makeBFArray = newGlobalBitField 

nextPiceToRequest :: Peer -> IO [(Int, Bool)]
nextPiceToRequest peer = atomically $ do global <- globalIndexArray peer
                                         bf <- bitFieldArray peer
                                         let difLs = arrayDiff global bf    
                                         difLs
                                              
          
arrayDiff:: (MArray a1 e m, MArray a e m, Ix i, Eq e) => a i e -> a1 i e -> m [(i, e)]
arrayDiff arr1 arr2 = do l1<-(getAssocs arr1)
                         l2<-(getAssocs arr2)
                         return (l1 \\ l2)            
                            
setNotVirgin :: Peer -> IO ()                       
setNotVirgin peer = modifyIORef' (amIVirgin peer) (\_->False)                         

setInterested :: Peer-> IO () 
setInterested peer = modifyIORef' (amIInterested peer) (\_->True) 

setNotInterested :: Peer-> IO () 
setNotInterested peer = modifyIORef' (amIInterested peer) (\_->False) 


updateBF :: Peer -> BC.ByteString -> IO ()
updateBF peer bf = atomically $ (bitFieldArray peer)>>=(\arr ->updateArray (arr) bf )
  
updateBFIndex :: Peer -> Int -> IO ()
updateBFIndex peer i = atomically $ (bitFieldArray peer)>>=(\arr ->writeArray arr i True)
         
canTalkToPeer :: Peer -> IO Bool
canTalkToPeer peer = do isVirgin <- readIORef (amIVirgin peer)    
                        isInterested <-readIORef (amIInterested peer)                     
                        isIChocked <- readIORef (amIChocked peer)
                        return $  isVirgin || (isIChocked && isInterested)               
                       
--showPeer :: Peer -> IO String                  
showPeer p= do let name = peerP p
             --  arr <-getElems (bitFieldArray p)    
               return name--(name, arr)
                         
                 
makePeer :: BC.ByteString -> N.HostName -> N.PortNumber -> Int -> IO Peer             
makePeer hash host port size = do handle<- N.connectTo host (N.PortNumber  port)
                                  SIO.hSetBuffering handle SIO.NoBuffering                            
                                --  SIO.hSetBuffering handle (SIO.BlockBuffering Nothing)
                                  --hSetBuffering handle LineBuffering
                                  sendHandshake handle hash $ BC.pack myId
                                  recvHandshake handle size                                        
                                    
             
sendHandshake :: SIO.Handle -> B.ByteString -> B.ByteString -> IO ()
sendHandshake handle hash peer = BC.hPutStr handle msg -- >> print "Handshake finished"
        where msg = B.concat [len, ptr, rsrv, hash, peer]
              len = B.singleton $ (fromIntegral . length) protocol
              ptr = BC.pack protocol
              rsrv = B.replicate 8 (fromIntegral 0)       
              
              
recvHandshake :: SIO.Handle-> Int -> IO Peer
recvHandshake handle size = do len <- B.hGet handle 1
                               ptr <- B.hGet handle $ fromBsToInt len
                               rsrv <- B.hGet handle 8
                               hash <- B.hGet handle 20
                               peer <- B.hGet handle 20 
                               amIVirgin <- newIORef True
                               amIChocked <- newIORef True
                               amIInterested <- newIORef False
                               let bfArr = makeBFArray size
                               let global = newGlobalBitField size
                               return $ Peer handle (BC.unpack peer) amIInterested amIChocked amIVirgin bfArr global

                                  
                    
updateArray:: (MArray a Bool m, Ix i, Num i) =>a i Bool -> BC.ByteString -> m ()
updateArray arr bs = update arr (convertToBits bs) 0   


update :: (MArray a e m, Ix i, Num i) => a i e -> [e] -> i -> m ()
update arr [] _ = return ()
update arr (x:xs) i = do (lo, hi) <- getBounds arr
                         if checkBounds (lo, hi) then
                            (writeArray arr i x) >> update arr xs (i+1)
                         else
                            return ()     
                      where checkBounds (lo,hi) = lo>=i && hi<=i
        
convertToBits bs = [Bits.testBit w i| w<-B.unpack bs, i<-[7,6.. 0]]
             
fromBsToInt bs = sum $ zipWith (\x y->x*2^y) (reverse ws) [0,8..]
                 where ws = map fromIntegral (B.unpack bs)               
                                                                        



                            
                            
               
