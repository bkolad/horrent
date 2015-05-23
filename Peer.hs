{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module Peer (Peer, makePeer, myId, showPeer, peerP, handleP, setInterested, 
setNotVirgin, getBitFieldList, {--canTalkToPeer,--} updateBF, fromBsToInt, 
updateBFIndex, amIInterested, bitFieldArray, nextPiceToRequest, nextBuffIdx, appendToBuffer, appendBuffToFile) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import qualified Network as N
import qualified System.IO as SIO
import qualified Data.Bits as Bits
import qualified Control.Concurrent.STM.TArray as TA
import qualified Data.Sequence as Seq
import Control.Concurrent.STM
import qualified Data.Foldable as F
import Data.IORef
import Data.List
import Control.Applicative
import Data.Array.MArray
import Types
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put



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
                , buffer :: IORef (Seq.Seq BC.ByteString)
                , nextBuffIdx ::IORef Int
                } 
                

newStm = atomically (newTVar False)
                   
  
makeBFArray ::Int-> IO Bitfield  
makeBFArray size = atomically $ newArray (0, size-1) False  

getBitFieldList peer = atomically $ getAssocs $ bitFieldArray peer 
      
appendToBuffer :: Peer -> BC.ByteString -> IO ()
appendToBuffer peer content = modifyIORef (buffer peer) (\bff->bff Seq.|> content)


appendBuffToFile :: Peer -> String ->IO()
appendBuffToFile peer fN = do buff <- readIORef $ buffer peer
                              E.bracket (SIO.openBinaryFile fN SIO.AppendMode) SIO.hClose 
                                        (\h->F.mapM_ (\x->BC.hPut h x) buff)
                            
  
nextPiceToRequest :: Peer -> IO [(Int, Bool)]
nextPiceToRequest peer = do atomically $ arrayDiff (globalIndexArray peer) (bitFieldArray peer)
                          
arrayDiff :: (MArray a1 PiceInfo m, MArray a Bool m, Applicative m, Ix i) => a1 i PiceInfo -> a i Bool -> m [(i, Bool)]
arrayDiff arr1 arr2 = do l1 <- (getAssocs arr1)
                         l2 <- (getAssocs arr2)
                         return $ ((isNotHave) <$> l1) \\ l2 
  where isNotHave n = case n of
                         (i, NotHave) -> (i, False)
                         (i, _)       -> (i, True)
                  
                   
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
                                                 SIO.hSetBinaryMode handle True
                                                 sendHandshake handle hash $ BC.pack myId
                                                 recvHandshake handle size globalPiceInfo                                      
                                                    
    
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
  
  
  
  
data Handshake = Handshake { len :: Int 
                           , peerProtocol :: String
                           , reserved :: B.ByteString
                           , hash :: B.ByteString
                           , peerName :: String
                           }              
  
instance Binary Handshake where  
  
  put handshake = putWord8 (fromIntegral . length $ protocol) 
               >> (putByteString $ BC.pack protocol) 
               >> (putWord64be 0)
               >> (putByteString $ hash handshake)
               >> (putByteString $ BC.pack $ peerName handshake)
  
  
  get = do len <- (fromBsToInt <$> getByteString 1)
           ptr <- BC.unpack <$> getByteString len
           rsrv <- getByteString 8
           hash <- getByteString 20
           peer <- BC.unpack <$> getByteString 20 
           return $ Handshake len ptr rsrv hash peer                       


sendHandshake :: SIO.Handle -> B.ByteString -> B.ByteString -> IO ()
sendHandshake handle hash peer = BL.hPutStr handle $ encode handshake -- >> print "Handshake finished"
  where handshake = Handshake len protocol rsrv hash myId
        len = length protocol
        rsrv = B.replicate 8 0            

getHandshake :: SIO.Handle -> IO Handshake 
getHandshake handle =  decode <$> (BL.hGetContents handle)
 
 {--
recvHandshake :: SIO.Handle-> Int -> GlobalPiceInfo -> IO Peer
recvHandshake handle size globalPiceInfo = do handshake <- getHandshake handle
                                              amIVirgin <- newIORef True
                                              amIChocked <- newIORef True
                                              amIInterested <- atomically (newTVar False)
                                              bfArr <- makeBFArray size
                                              return $ Peer handle (peerName handshake) amIInterested amIChocked amIVirgin bfArr globalPiceInfo
 
 --}                 
recvHandshake :: SIO.Handle-> Int -> GlobalPiceInfo -> IO Peer
recvHandshake handle size globalPiceInfo = do len <- B.hGet handle 1
                                              ptr <- B.hGet handle $ fromBsToInt len
                                              rsrv <- B.hGet handle 8
                                              hash <- B.hGet handle 20
                                              peer <- B.hGet handle 20 
                                              amIVirgin <- newIORef True
                                              amIChocked <- newIORef True
                                              idx <- newIORef 32
                                              sq<-newIORef Seq.empty
                                              amIInterested <- atomically (newTVar False)
                                              bfArr <- makeBFArray size
                                              return $ Peer handle 
                                                            (BC.unpack peer) 
                                                            amIInterested 
                                                            amIChocked 
                                                            amIVirgin 
                                                            bfArr 
                                                            globalPiceInfo 
                                                            sq
                                                            idx

                            
                            
               
