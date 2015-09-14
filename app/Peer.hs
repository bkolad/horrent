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
                 , unChoked :: Bool
                 , buffer :: Maybe (Int, Int, B.ByteString)
                 }

                 
instance Show Peer where
  show p = (hostName p) ++ " "++ (show $ port p) ++ " " ++ (show $ pieces p)                
                 
                 
                 
fromBsToInt bs = 
   sum $ zipWith (\x y->x*2^y) (reverse ws) [0,8..]
   where 
      ws = map fromIntegral (B.unpack bs)

                       
convertToBits bs = 
   map snd $ filter fst $ zip bits [0 ..] 
   where
     bits = [Bits.testBit w i| w <- B.unpack bs, i <- [7,6.. 0]]
     
    
  
  
requestNext :: Peer -> IO (Maybe Int)
requestNext peer =
   STM.atomically $ let pics = pieces peer
                        global = globalStatus peer
                    in reqNext pics global
  



reqNext :: [Int] -> TP.GlobalPiceInfo -> STM.STM (Maybe Int)  
reqNext [] _ = return Nothing     
reqNext (x:xs) global = 
   do pInfo <- MA.readArray global x
      case pInfo of
           TP.NotHave    -> return $ Just x
           TP.InProgress -> reqNext xs global
           TP.Done       -> reqNext xs global
  
      