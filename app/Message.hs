module Message where

-- import qualified Peer as P (Peer, makePeer, showPeer, fromBsToInt, handleP) 
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified System.IO as SIO
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put


type MsgLen = Word32

type MsgPayload = B.ByteString


data Message = KeepAlive 
             | Choke
             | UnChoke
             | Interested
             | NotInterested
             | Have B.ByteString
             | Bitfield B.ByteString
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
   
   
   
   get = do 
            numBytes <- fromIntegral <$> getWord32be
            if (numBytes == 0)
               then return KeepAlive
               else do idx <- fromIntegral <$>  getWord8
                       
                       let size =(numBytes -1) 
                                                                       
                       bs <- getByteString size
                       case idx of
                             0 -> return Choke
                             1 -> return UnChoke
                             2 -> return Interested
                             3 -> return NotInterested
                             4 -> return $ Have bs
                             5 -> return $ Bitfield bs
                             6 -> return $ Request (undefined, undefined, undefined)
                             7 -> return $ Piece (toPiece bs)
                             8 -> return $ Cancel
                             9 -> return $ Port
                             x -> return $ Unknown (fromIntegral size) (fromIntegral x)
       
  
  
getM :: Get Message
getM =  do 
            numBytes <- fromIntegral <$> getWord32be
            if (numBytes == 0)
               then return KeepAlive
               else do idx <- fromIntegral <$>  getWord8
                       
                       let size =(numBytes -1) 
                                                                       
                       bs <- getByteString size
                       case idx of
                             0 -> return Choke
                             1 -> return UnChoke
                             2 -> return Interested
                             3 -> return NotInterested
                             4 -> return $ Have bs
                             5 -> return $ Bitfield bs
                             6 -> return $ Request (undefined, undefined, undefined)
                             7 -> return $ Piece (toPiece bs)
                             8 -> return $ Cancel
                             9 -> return $ Port
                             x -> return $ Unknown (fromIntegral size) (fromIntegral x)
       

decodeMessage :: B.ByteString -> Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, Message)
decodeMessage bs = decodeOrFail $ BL.fromStrict bs
  
  {--
decodeMessage1 bs = 
  case getMessage of
    (Fail _ _ _) -> Left "Failed"
    Partial fun -> undefined
    (Done _ _ x) -> Right x--}
    
  
  
  
getMessage :: Decoder Message   
getMessage = runGetIncremental get  
  
  {--case (runGetIncremental getM) of
                        Fail bs offset m -> Left ("Message Parsing Failed "++ (show m))
                        Partial f ->  Left ("Partial Parsing ")  --f (BL.fromStrict bs)   -- (decodeOrFail $ BL.fromStrict bs)
                        Done bs offset m -> Right (BL.fromStrict bs, offset, m)   --}
            
       
toPiece bs = runGet getTripplet (BL.fromChunks [bs])
  where getTripplet :: Get (Int, Int, B.ByteString)
        getTripplet = do numBytes <- fromIntegral <$> getWord32be
                         begin <- fromIntegral <$> getWord32be
                         rest <- getRemainingLazyByteString
                         return $ (numBytes, begin, (B.concat . BL.toChunks) rest)
   
   
  
encodeMessage :: Message -> B.ByteString
encodeMessage = BL.toStrict . encode
 
 
        
                 
