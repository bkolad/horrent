{-# LANGUAGE RankNTypes #-}

module Bencode.BParser
    ( BEncode
    , bStrL
    , bIntL
    , keyL
    , idL
    , bencode2ByteString
    , parse2BEncode
    , listL
    , uDPAnnounceParser
    ) where


import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative ((<*>), (<|>))
import Control.Lens --(Prism', prism, para)



data BEncode = BStr BC.ByteString
             | BInt Int
             | BList [BEncode]
             | BDic (Map.Map BEncode BEncode)
             deriving (Eq, Show, Ord)


instance Plated BEncode where
    plate f ec =
        case ec of
            (BList ls) -> BList <$> traverse f ls
            (BDic dic) -> BDic <$> tvk f dic
            k -> pure k
        where
            tvk fun dic =
                Map.fromList <$> traverse (cvt fun) (Map.toList dic)

            cvt fun (k, v) =
                (,) <$> fun k <*> fun v


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
            case Map.lookup (BStr (BC.pack k)) d of
                Just v -> Right v
                Nothing -> Left dc
    --    f ls@(BList ds) = BList <$> (sequence (f <$> ds))

        f e = Left e

listL :: Prism' BEncode [BEncode]
listL = prism BList f
    where
        f (BList ls) = Right ls
        f l = Left l


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


dicEntry :: Parser (BEncode, BEncode)
dicEntry = (,) <$> bStr <*> bencodeParser


bDic :: Parser BEncode
bDic = BDic . Map.fromList <$> (char 'd' *> many1 dicEntry <* char 'e')


bencodeParser :: Parser BEncode
bencodeParser = bInt <|> bStr <|> bList <|> bDic


parse2BEncode :: BC.ByteString -> Either String BEncode
parse2BEncode = P.parseOnly bencodeParser

bencode2ByteString :: BEncode -> BC.ByteString
bencode2ByteString dic = para convertToBS dic


convertToBS :: BEncode -> [BC.ByteString] -> BC.ByteString
convertToBS dic kls =
    case dic of
        (BInt i)   -> BC.pack ("i" ++ show i ++ "e")
        (BStr  bs) -> B.concat [BC.pack $ show (B.length bs) ++ ":", bs]
        (BList ls) -> B.concat [BC.pack "l", B.concat kls, BC.pack "e"]
        (BDic dic) -> B.concat [BC.pack "d", B.concat kls, BC.pack "e"]


uDPAnnounceParser :: Parser (BC.ByteString, BC.ByteString)
uDPAnnounceParser = do
    udpH <- string (BC.pack "udp://")
            *> P.takeWhile (/= ':')
            <* char ':'
    port <- num
    return (udpH ,BC.pack port)
