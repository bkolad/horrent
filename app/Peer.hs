module Peer where

import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import qualified Network as N
import qualified Types as TP
import qualified Data.Word as W
import qualified Data.List as L


data Peer = Peer { hostName :: N.HostName
                 , port :: Int
                 , pieces :: [Int]
                 , infoHash :: B.ByteString
                 , globalStatus :: TP.GlobalPiceInfo
                 , unChoked :: Bool
                 , buffer :: B.ByteString 
                 , peceHashes :: TP.Buffer
                 , sizeInfo :: (TP.NumberOfPieces, TP.NormalPieceSize, TP.LastPieceSize)
                 }

                 
instance Show Peer where
  show p = (hostName p) ++ " "++ (show $ port p) ++ " " ++ (show $ pieces p)                
                 
                 
                 
fromBsToInt bs = 
   sum $ zipWith (\x y->x*2^y) (reverse ws) [0,8..]
   where 
      ws = map fromIntegral (B.unpack bs)

      
      
convertToBits :: B.ByteString -> [Int]      
convertToBits bs = 
   map snd $ filter fst $ zip bits [0 ..] 
   where
     bits = [Bits.testBit w i| w <- B.unpack bs, i <- [7,6.. 0]]
     
     
     
     
  
  
     
intsToBools :: [Int] -> Int -> [Bool]
intsToBools [] idx = []
intsToBools (x : xs) idx = if (idx == x)
                              then True : (intsToBools xs (idx+1))
                              else False : (intsToBools (x:xs) (idx+1))

groupBools [] = [[]] 
groupBools ls = let (ls8, rest) = L.splitAt 8 ls
                in ls8 : (groupBools rest)
 
                              
                              
boolsToW8 :: [Bool] -> W.Word8
boolsToW8 ls = L.foldl'(\acc (i, b)-> acc + (if b then (2^ (7-i)) else 0) ) 0 (zip [0 ..] ls)
    

hasIdxs :: [Int] -> B.ByteString
hasIdxs ls = B.pack $ init $  map boolsToW8 $ groupBools (intsToBools ls 0)     
    
    
    
ls = [True, False, False, False, False, False, False, True]  
  
kk = intsToBools [1,6,7] 0 


        



  
      