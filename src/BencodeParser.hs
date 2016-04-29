{-# LANGUAGE RankNTypes #-}

module BencodeParser ( BEncode
                     , bStrL
                     , bIntL
                     , keyL
                     , idL
                     , toByteString
                     , bencodeParser
                     ) where


import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative
import Control.Lens


data BEncode = BStr BC.ByteString
             | BInt Int
             | BList [BEncode]
             | BDic (Map.Map BC.ByteString BEncode)
             deriving (Eq, Show)


idL :: Prism' BEncode BEncode
idL = prism id Right

bStrL :: Prism' BEncode BC.ByteString
bStrL = prism BStr f
    where
        f (BStr s) = Right s
        f l = Left l


bIntL :: Prism' BEncode Int
bIntL = prism BInt f
    where
        f (BInt i) = Right i
        f l = Left l


keyL :: String -> Prism' BEncode BEncode
keyL k = prism id f
    where
        f dc@(BDic d) =
            case Map.lookup (BC.pack k) d of
                Just v -> Right v
                Nothing -> Left dc
        f e = Left e


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
bDic = BDic . Map.fromList <$> (char 'd' *> many1 dicEntry <* char 'e')


bencodeParser :: Parser BEncode
bencodeParser = bInt <|> bStr <|> bList <|> bDic


toByteString::BEncode -> BC.ByteString
toByteString (BInt i)   = B.concat $ map BC.pack ["i", show i, "e"]
toByteString (BStr  bs) = B.concat [BC.pack(show (B.length bs) ++":"), bs]
toByteString (BList ls) = BC.concat [BC.pack "l", BC.concat (map toByteString ls), BC.pack "e"]
toByteString (BDic dic) = B.concat [BC.pack "d", dBS dic,BC.pack "e"]
    where
        dBS dic = Map.foldlWithKey' entryToBS BC.empty dic
        entryToBS acc k v = B.concat [acc, toByteString $ BStr k ,toByteString v]

--toByteString (BDic [])=BC.pack ""
--toByteString (BDic ls) = B.concat [BC.pack "d",entryToBS ls ,BC.pack "e"]
 -- where entryToBS [] = BC.pack ""
    --    entryToBS ((k,v):xs)= B.concat [toByteString $ BStr k ,toByteString v, entryToBS xs]
