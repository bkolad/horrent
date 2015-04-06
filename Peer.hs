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


myId = "-TR2840-d0p22uiake0b" 
protocol = "BitTorrent protocol"


 
data Peer = Peer{ handleP :: SIO.Handle
                , peerP :: String 
                , amIInterested :: IORef Bool -- false
                , amIChocked :: IORef Bool  -- true
                , amIVirgin :: IORef Bool -- first time I am talking to a peer
                , bitFieldArray :: IOArray Int Bool
                } 
                
type GlobalBitfield = STM (TA.TArray Int Bool)
                
newGlobalBitField ::Int-> GlobalBitfield
newGlobalBitField size= newArray (0, size) False     
                       
setNotVirgin :: Peer -> IO ()                       
setNotVirgin peer = modifyIORef' (amIVirgin peer) (\_->False)                         

setInterested :: Peer-> Bool-> IO () 
setInterested peer b = modifyIORef' (amIInterested peer) (\_->b) 
  
updateBF :: MArray IOArray Bool m => Peer -> BC.ByteString -> m ()  
updateBF peer bf = updateArray (bitFieldArray peer) bf         
  
updateBFIndex :: MArray IOArray Bool m => Peer -> Int -> m ()
updateBFIndex peer i = writeArray (bitFieldArray peer) i True  
         
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
                                --   hSetBuffering handle (BlockBuffering Nothing)
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
                               bfArr <- makeBFArray size
                               return $ Peer handle (BC.unpack peer) amIInterested amIChocked amIVirgin bfArr

makeBFArray size = newArray (0,size-1) False :: IO (IOArray Int Bool)
                                  
                    
--updateArray :: IOArray Int Bool -> B.ByteString -> IO ()   
updateArray arr bs = update arr (convertToBits bs) 0   


--update :: MArray Int Bool -> [Bool] -> Int -> IO ()
--update :: IOArray Int Bool -> [Bool] -> Int -> IO ()
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
                                                                        



                            
                            
               
