module BencodeParser where


import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative


data BEncode = BStr BC.ByteString
             | BInt Int
             | BList [BEncode]
             | BDic (Map.Map BC.ByteString BEncode)
             deriving (Eq, Show)

num::Parser String
num = many1 digit

bString :: Parser BC.ByteString
bString = do n <- num
             _ <- char ':'
             P.take $ read n

bInt::Parser BEncode
bInt = BInt . read <$> (char 'i' *> num <* char 'e')

bStr :: Parser BEncode
bStr = BStr <$> bString

bList :: Parser BEncode
bList = BList <$> (char 'l' *> vals <* char 'e')
    where
        vals = many1 $ bInt <|> bStr <|> bList <|> bDic

dicEntry :: Parser (BC.ByteString, BEncode)
dicEntry = (,) <$> bString <*> bencodeParser

bDic :: Parser BEncode
bDic = BDic . insert <$>(char 'd' *> many1 dicEntry <* char 'e')
    where
        insert = foldl (\acc (k, v) -> Map.insert k v acc) Map.empty

bencodeParser :: Parser BEncode
bencodeParser = bInt <|> bStr <|> bList <|> bDic
