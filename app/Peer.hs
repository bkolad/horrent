module Peer ( fromBsToInt
            , pieceLsToBS
            , bsToPieceLs
            , Peer (..)
            , hashFor) where

import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import qualified Network as N
import qualified Types as TP
import qualified Data.Word as W
import qualified Data.List as L
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Map as Map



data Peer = Peer { hostName :: N.HostName
                 , port :: Int
                 , pieces :: [Int]
                 , infoHash :: B.ByteString
                 , globalStatus :: TP.GlobalPiceInfo
                 , unChoked :: Bool
                 , buffer :: B.ByteString
                 , pieceHashes :: TP.HashInfo
                 , sizeInfo :: (TP.NumberOfPieces, TP.NormalPieceSize, TP.LastPieceSize)
                 }


instance Show Peer where
  show p = (hostName p) ++ " "++ (show $ port p) ++ " " ++ (show $ pieces p)



fromBsToInt bs =
   sum $ zipWith (\x y -> x*2^y) (reverse ws) [0,8..]
   where
      ws = map fromIntegral (B.unpack bs)


pieceLsToBS :: [Int] -> B.ByteString
pieceLsToBS ls =
    B.pack $ (Map.elems . fillEmptyKyes . mkMap) ls


fillEmptyKyes ::  Map.Map Int W.Word8 -> Map.Map Int W.Word8
fillEmptyKyes m =  L.foldl' fillEmpty m [0.. maxK]
    where
        fillEmpty acc k = Map.insertWith (+) k 0 acc
        (maxK, _) = Map.findMax m


mkMap :: [Int] -> Map.Map Int W.Word8
mkMap ls = L.foldl' toMap Map.empty ls
    where
        toMap acc x =
            let k = div x 8
                i = 7 - mod x 8
            in Map.insertWith (+) k (2^i) acc


bsToPieceLs :: B.ByteString -> [Int]
bsToPieceLs bs =
   map snd $ filter fst $ zip bits [0 ..]
   where
       bits = [Bits.testBit w i| w <- B.unpack bs, i <- [7,6.. 0]]


hashFor = SHA1.hash
