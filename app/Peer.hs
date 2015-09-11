module Peer where

import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import qualified Network as N
import qualified Types as TP
import qualified Data.Array.MArray as MA
import qualified Control.Concurrent.STM as STM


data Peer = Peer { hostName :: N.HostName
                 , port :: Int
                 , pieces :: [Int]
                 , infoHash :: B.ByteString
                 , globalStatus :: TP.GlobalPiceInfo
                 }

fromBsToInt bs = 
   sum $ zipWith (\x y->x*2^y) (reverse ws) [0,8..]
   where 
      ws = map fromIntegral (B.unpack bs)

                       
convertToBits bs = 
   map snd $ filter fst $ zip bits [0 ..] 
   where
     bits = [Bits.testBit w i| w <- B.unpack bs, i <- [7,6.. 0]]
     
  
  
isInteresting :: Peer -> IO Bool  
isInteresting peer = 
  STM.atomically $ let pics = pieces peer
                       global = globalStatus peer
                   in interesting pics global
  
  
  
interesting :: [Int] -> TP.GlobalPiceInfo -> STM.STM (Bool)
interesting [] _ = return False
interesting (x:xs) global = 
  do pInfo <- MA.readArray global x
     case pInfo of
          TP.NotHave    -> return True
          TP.InProgress -> interesting xs global
          TP.Done       -> interesting xs global
        
        