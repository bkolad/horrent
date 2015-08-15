module Message where

import qualified Peer as P (Peer, makePeer, showPeer, fromBsToInt, handleP) 
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified System.IO as SIO
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative




type MsgLen = Word32

type MsgPayload = B.ByteString


data Message = KeepAlive 
             | Choke
             | UnChoke
             | Interested
             | NotInterested
             | Have (Int, B.ByteString)
             | Bitfield MsgPayload
             | Request (Int, Int, Int)
             | Piece (Int, Int, B.ByteString)
             | Cancel
             | Port 
             | Unknown MsgLen Int
             deriving (Show)
             
                            


instance Binary Message where
  
   put KeepAlive           = putWord32be 0
   put Choke               = putWord32be 1 >> putWord8 0
   put UnChoke             = putWord32be 1 >> putWord8 1
   put Interested          = putWord32be 1 >> putWord8 2
   put NotInterested       = putWord32be 1 >> putWord8 3
   put (Request (i, o, l)) = putWord32be 13 >> putWord8 6 >> put32Int i 
                                                          >> put32Int o 
                                                          >> put32Int l
                                 
       where  put32Int = putWord32be . fromIntegral    
   
   
   put (Piece _)           = undefined
   put Cancel              = putWord32be 13 >> putWord8 8
   put Port                = putWord32be 3 >> putWord8 9
   put (Unknown _ _)       = undefined 
   
   
   
   get = do numBytes <- fromIntegral <$> getWord32be
            if (numBytes == 0)
               then return KeepAlive
               else do idx <- getWord8
                       let size = numBytes -1  
                       bs <- getByteString size
                       case idx of
                             0 -> return Choke
                             1 -> return UnChoke
                             2 -> return Interested
                             3 -> return NotInterested
                             4 -> return $ Have (P.fromBsToInt bs, bs)
                             5 -> return $ Bitfield bs
                             6 -> return $ Request (undefined, undefined, undefined)
                             7 -> return $ Piece (toPiece bs)
                             8 -> return $ Cancel
                             9 -> return $ Port
                             x -> return $ Unknown (fromIntegral size) (fromIntegral x)
       
       
toPiece bs = runGet getTripplet (BL.fromChunks [bs])
  where getTripplet :: Get (Int, Int, B.ByteString)
        getTripplet = do idx <-  getWord32be
                         begin <- getWord32be
                         rest <- getRemainingLazyByteString
                         return $ (fromIntegral idx, fromIntegral begin, (B.concat . BL.toChunks) rest)
   
   
getMessage :: SIO.Handle -> IO Message 
getMessage handle =  decode <$> (BL.hGetContents handle)
                                                         
                  
sendMsg :: P.Peer -> Message -> IO ()
sendMsg peer msg = send msg
  where handle = P.handleP peer
        send = BL.hPutStr handle . encode  
                          
                 
