{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module Peer (Peer, makePeer, showPeer, peerP, handleP, setInterested, 
setNotVirgin, getBitFieldList, {--canTalkToPeer,--} updateBF, fromBsToInt, 
updateBFIndex, amIInterested, bitFieldArray, nextPiceToRequest, nextBuffIdx, appendToBuffer, appendBuffToFile, getBuffer2BS, hashes, buffer) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import qualified Network as N
import qualified System.IO as SIO
import qualified Data.Bits as Bits
import qualified Control.Concurrent.STM.TArray as TA
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import qualified Data.Monoid as M
import Control.Concurrent.STM
import Data.IORef
import Data.List
import Control.Applicative
import Data.Array.MArray
import Types

type Bitfield = TA.TArray Int Bool 

data Progress = NextId Int | Finished

data Peer = Peer{ handleP :: SIO.Handle
                , peerP :: String 
                , amIInterested :: TVar Bool -- false
                , amIChocked :: IORef Bool  -- true
                , amIVirgin :: IORef Bool -- first time I am talking to a peer
                , bitFieldArray :: Bitfield
                , globalIndexArray :: GlobalPiceInfo -- TO do diffrent statuste req, pending, done 
                , buffer :: IORef Buffer
                , nextBuffIdx ::IORef Int
                , hashes :: Buffer
                } 
                

newStm = atomically (newTVar False)
                   
  
makeBFArray ::Int-> IO Bitfield  
makeBFArray size = atomically $ newArray (0, size-1) False  

getBitFieldList peer = atomically $ getAssocs $ bitFieldArray peer 
      
appendToBuffer :: Peer -> BC.ByteString -> IO ()
appendToBuffer peer content = modifyIORef (buffer peer) (\bff->bff Seq.|> content)

getBuffer2BS :: Buffer -> BC.ByteString
getBuffer2BS  = F.foldr M.mappend M.mempty  



appendBuffToFile :: Peer -> String ->IO()
appendBuffToFile peer fN = do buff <- readIORef $ buffer peer
                              E.bracket (SIO.openBinaryFile ("downloads/"++fN) SIO.WriteMode) SIO.hClose 
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


updateBF :: Peer -> BC.ByteString -> IO ()
updateBF peer bf = atomically $ updateArray (bitFieldArray peer) bf 
  
updateBFIndex :: Peer -> Int -> IO ()
updateBFIndex peer i = atomically $  writeArray (bitFieldArray peer) i True
                                             
                          
                         
makePeer :: SIO.Handle -> String -> Int -> GlobalPiceInfo -> Seq.Seq BC.ByteString -> IO Peer
makePeer handle peerName size globalPiceInfo hashes= do amIVirgin <- newIORef True
                                                        amIChocked <- newIORef True
                                                        idx <- newIORef 32
                                                        sq<-newIORef Seq.empty
                                                        amIInterested <- atomically (newTVar False)
                                                        bfArr <- makeBFArray size
                                                        return $ Peer handle peerName amIInterested amIChocked 
                                                                      amIVirgin bfArr globalPiceInfo sq idx hashes   
                                    
  
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
  
  
--showPeer :: Peer -> IO String                  
showPeer p= do let name = peerP p
             --  arr <-getElems (bitFieldArray p)    
               return name--(name, arr)
  
--setNotInterested :: Peer-> IO () 
--setNotInterested peer = atomically $ writeTVar (amIInterested peer) False 

                       {--    
canTalkToPeer :: Peer -> IO Bool
canTalkToPeer peer = do isVirgin <- readIORef (amIVirgin peer)    
                        isInterested <-readIORef (amIInterested peer)                     
                        isIChocked <- readIORef (amIChocked peer)
                        return $  isVirgin || (isIChocked && isInterested)               
                        --}
