module Peer where

import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import qualified Network as N
import qualified Types as TP


data Peer = Peer { hostName :: N.HostName
                 , port :: Int
                 , pieces :: [Int]
                 , infoHash :: B.ByteString
                 , globalStatus :: TP.GlobalPiceInfo
                 , unChoked :: Bool
                 , buffer :: Maybe (Int, Int, B.ByteString)
                 , sizeInfo :: (TP.NumberOfPieces, TP.NormalPieceSize, TP.LastPieceSize)
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
     
    
  
  


        



  
      