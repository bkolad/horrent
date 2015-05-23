module BencodeParser (BEncode, annouce, infoHash, parseFromFile, parseFromBS, peers, piceSize, torrentSize) where

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Either
import Control.Applicative
import qualified Crypto.Hash.SHA1 as SHA1 (hash)
import Control.Monad.Error
import qualified Data.ByteString.Base16 as B16


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

pieceHash :: BEncode -> Either String BC.ByteString
pieceHash inDic = do infoDic <- find "info" inDic
                     (BStr (BString bStr)) <- find "pieces" infoDic
                     return bStr 

splitEvery :: Int -> BC.ByteString -> [BC.ByteString]                     
splitEvery n bc = if (BC.null bc)
                     then []
                     else s:(splitEvery n e)
                  where (s,e) = BC.splitAt n bc                      

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
bList = (BList) <$> (char 'l' *> (many1 (bInt <|> (BStr <$> bString) <|> bList)) <* char 'e')                  
                  
                    
dicEntry :: P.Parser (BStringT, BEncode)
dicEntry = ((,)<$>bString <*> bencodeParser)
                    
bDic :: P.Parser BEncode
bDic = BDic<$>((char 'd' *> many1 dicEntry <* char 'e'))               
                  
bencodeParser :: P.Parser BEncode
bencodeParser = bInt <|> (BStr <$> bString) <|> bList <|> bDic



{--TESTS--}

printer::IO()
printer = do content <- parseFromFile "ubuntu.torrent"
             case content of
                  Left l -> print $ "Problem with reading torrent file" ++ (show l)
                  Right dic -> print dic  
  
  
infoPrinter::IO()
infoPrinter = do content <- parseFromFile "ubuntu.torrent"
                 case content >>= (find "info") of
                      Left e-> print e
                      Right c  -> print (toByteString c)
                                    
piecesPrinter::IO()
piecesPrinter = do content <- parseFromFile "ubuntu.torrent"                                    
                   case content>>=pieceHash of
                      Left e-> print e
                      Right c  -> print $ B16.encode ((splitEvery 20 c) !! 10)


  
prettyPrint:: BEncode -> IO()
prettyPrint (BInt i) = putStrLn (show i)
prettyPrint (BStr s) = putStrLn (show s)
prettyPrint (BList []) = return ()
prettyPrint (BList (x:xs)) = (prettyPrint x) >>= (\_->prettyPrint (BList xs))
prettyPrint (BDic []) = return ()
prettyPrint (BDic ((key, value):xs)) = prettyPrint (BStr key)  >>= (\_->prettyPrint (value))  >>= (\_->prettyPrint (BDic xs)) 
