module Peer where

import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import qualified Network as N
import qualified Types as TP
import qualified Data.Word as W
import qualified Data.List as L
import qualified Crypto.Hash.SHA1 as SHA1


data Peer = Peer { hostName :: N.HostName
                 , port :: Int
                 , pieces :: [Int]
                 , infoHash :: B.ByteString
                 , globalStatus :: TP.GlobalPiceInfo
                 , unChoked :: Bool
                 , buffer :: B.ByteString
                 , peceHashes :: TP.HashInfo
                 , sizeInfo :: (TP.NumberOfPieces, TP.NormalPieceSize, TP.LastPieceSize)
                 }


instance Show Peer where
  show p = (hostName p) ++ " "++ (show $ port p) ++ " " ++ (show $ pieces p)



fromBsToInt bs =
   sum $ zipWith (\x y->x*2^y) (reverse ws) [0,8..]
   where
      ws = map fromIntegral (B.unpack bs)


convertBitsToBs :: [Int] ->  B.ByteString
convertBitsToBs ls = let l = l8 ls
                         lz = map (zip [7,6 ..0 ]) l
                         lzz = map (filter (\(i, x)-> x/=0)) lz
                         lzzz = map (map (\(i,x)->2^i)) lzz
                         lk = map (sum) lzzz
                     in B.pack $ map (fromInteger)lk

l8 :: [a] -> [[a]]
l8 [] = []
l8 ls = let (l, r) = splitAt 8 ls
        in l : l8 r

convertToBits :: B.ByteString -> [Int]
convertToBits bs =
   map snd $ filter fst $ zip bits [0 ..]
   where
     bits = [Bits.testBit w i| w <- B.unpack bs, i <- [7,6.. 0]]




hashFor = SHA1.hash


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
