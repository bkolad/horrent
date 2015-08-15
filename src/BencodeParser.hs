module BencodeParser (BEncode, annouce, infoHash, parseFromFile, parseFromBS, peers, piceSize, torrentSize, piecesHashSeq) where

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Crypto.Hash.SHA1 as SHA1 (hash)
import qualified Data.ByteString.Base16 as B16
import qualified Data.Sequence as Seq

import Data.Attoparsec.ByteString.Char8
import Data.Either
import Control.Applicative
import Types
import Debug.Trace

data BStringT = BString BC.ByteString deriving (Eq, Show)

data BEncode = BStr BStringT
             | BInt Int
             | BList [BEncode]
             | BDic [(BStringT, BEncode)]
             deriving (Eq, Show) 
             
{--PUBLIC--} 

annouce :: BEncode ->Either String String
annouce inDic =  BC.unpack <$> ((find "announce" inDic) >>= getBStr) 

peers :: BEncode -> Either String BC.ByteString       
peers inDic =  ((find "peers" inDic) >>= getBStr)

piecesHash :: BEncode -> Either String BC.ByteString
piecesHash inDic = do infoDic <- find "info" inDic
                      (BStr (BString bStr)) <- find "pieces" infoDic
                      return bStr 
                      
                      
splitEvery :: Int -> BC.ByteString -> Buffer
splitEvery n bc = if (BC.null bc)
                     then Seq.empty
                     else s Seq.<| (splitEvery n e)
                  where (s,e) = BC.splitAt n bc 

                  
piecesHashSeq :: BEncode -> Either String Buffer         
piecesHashSeq dic = (splitEvery 20) <$> piecesHash dic 

piceSize :: BEncode ->Either String Int
piceSize inDic = do  infoDic <- find "info" inDic
                     (BInt len) <- find "piece length" infoDic
                     return len 

torrentSize :: BEncode ->Either String Int
torrentSize inDic = do infoDic <- find "info" inDic
                       (BInt len) <- find "length" infoDic
                       return len 
                                         

infoHash :: BEncode -> Either String String
infoHash inDic = (BC.unpack . SHA1.hash . toByteString) <$> (find "info" inDic) 

parseFromFile::String -> IO (Either String BEncode)
parseFromFile path = B.readFile path >>= \x-> return $ P.parseOnly bencodeParser x


parseFromBS :: B.ByteString -> Either String BEncode
parseFromBS x = P.parseOnly bencodeParser x
 
getBStr (BStr (BString bs)) = Right bs
getBStr _ = Left "Not a String"
 
 
{--PRIVATE--} 
        
find :: String -> BEncode -> Either String  BEncode
find keyS (BDic ls) = mHead $ filtered ls
   where filtered l = filter (\(k,v)-> k == key) l
         key =  BString $ BC.pack keyS
         mHead [] = Left $ "Couldn't find "++keyS++" key"
         mHead ((x1,x2):xs) = Right x2
find keyS _ = Left "Give me dictionary!"


toByteString::BEncode->BC.ByteString
toByteString (BInt i) = B.concat $ map BC.pack ["i", show i, "e"]
toByteString (BStr (BString bs)) = B.concat [BC.pack(show (B.length bs) ++":"), bs]
toByteString (BList ls) = BC.concat [BC.pack "l", BC.concat (map toByteString ls), BC.pack "e"]
toByteString (BDic [])=BC.pack ""
toByteString (BDic ls) = B.concat [BC.pack "d",entryToBS ls ,BC.pack "e"] 
  where entryToBS [] = BC.pack ""
        entryToBS ((k,v):xs)= B.concat [toByteString $ BStr k ,toByteString v, entryToBS xs]



{--PARSERS--}

num::P.Parser String             
num = many1 digit

bInt::P.Parser BEncode
bInt = (BInt . read) <$> (char 'i' *> num <* char 'e') 

bString :: P.Parser BStringT
bString = do n <- num
             _ <- char ':'
             BString <$> (P.take (read n))
             
bList :: P.Parser BEncode
bList = (BList) <$> (char 'l' *> (many1 (bInt <|> (BStr <$> bString) <|> bList <|> bDic)) <* char 'e')                  
                  
                    
dicEntry :: P.Parser (BStringT, BEncode)
dicEntry = ((,)<$>bString <*> bencodeParser)
                    
bDic :: P.Parser BEncode
bDic = BDic<$>((char 'd' *> many1 dicEntry <* char 'e'))               
                  
bencodeParser :: P.Parser BEncode
bencodeParser = bInt <|> (BStr <$> bString) <|> bList <|> bDic



{--TESTS--}

torrent = "tom.torrent"--"karl_marx.torrent"--"tom.torrent"--"tom.torrent" -- "karl_marx.torrent"--"tom.torrent" -- 

printer::IO()
printer = do content <- parseFromFile torrent
             case content of
                  Left l -> print $ "Problem with reading torrent file" ++ (show l)
                  Right dic -> putStr $ prettyPrint 0 dic  
  
  
infoPrinter::IO()
infoPrinter = do content <- parseFromFile torrent
                 case content >>= (find "info") of
                      Left e-> print e
                      Right c  -> print (toByteString c)
                                    
piecesPrinter::IO()
piecesPrinter = do content <- parseFromFile torrent                                   
                   case content>>=piecesHashSeq of
                      Left e-> print e
                      Right c  -> print $ B16.encode (Seq.index c 10)

 
emptySpace n =  concat $ replicate n "  "


nl = ("\n")

prettyPrint :: Int -> BEncode -> String
prettyPrint n (BInt i) = show i
prettyPrint n (BStr (BString s)) = show s
prettyPrint n (BList []) = ""
prettyPrint n (BList (x:xs)) = nl ++ (emptySpace (n+3)) 
                                   ++ (prettyPrint (n+1) x)  
                                   ++ (prettyPrint n (BList xs)) 
prettyPrint n (BDic []) = ""
prettyPrint n (BDic ((BString key, value):xs)) = nl ++ (emptySpace n) 
                                                     ++ (show key) ++ " ::-> " 
                                                     ++ (prettyPrint (n+1) value)  
                                                     ++ (prettyPrint n (BDic xs)) 


