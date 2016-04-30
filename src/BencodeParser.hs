{-# LANGUAGE RankNTypes, GADTs #-}

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
             | BDic (Map.Map BEncode BEncode)
             deriving (Eq, Show, Ord)


instance Plated BEncode where
    plate f (BList ls) = BList <$> (traverse f ls)
    plate f (BDic dic) =
        BDic <$> --Map.fromList $ traverse (\(k, v) -> (\x -> (k,x)) <$> f v) (Map.toList dic)

        tvk f dic
    --    tvk = Map.fromList $ traverse (\(k, v) -> (,) k $ f k v) (Map.toList dic)
    plate _ x = pure x

tvk fun dic = Map.fromList <$> (traverse (\(k, v) ->  (,)<$>(fun k) <*> (fun v)) (Map.toList dic))


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


dicEntry :: Parser (BEncode, BEncode)
dicEntry = (,) <$> bStr <*> bencodeParser


bDic :: Parser BEncode
bDic = BDic . Map.fromList <$> (char 'd' *> many1 dicEntry <* char 'e')

bencodeParser :: Parser BEncode
bencodeParser = bInt <|> bStr <|> bList <|> bDic


toByteString dic = para convertToBS dic

convertToBS :: BEncode -> [BC.ByteString] -> BC.ByteString
convertToBS dic kls =
    case dic of
        (BInt i)   -> BC.pack ("i" ++ show i ++"e")
        (BStr  bs) -> B.concat [BC.pack (show (B.length bs) ++":"), bs]
        (BList ls) -> B.concat [BC.pack "l", B.concat kls, BC.pack "e"]
        (BDic dic) -> B.concat [BC.pack "d", B.concat kls, BC.pack "e"]


ben = BDic $ Map.singleton (BStr (BC.pack "pk")) (BList [BInt 1, BInt 2])

sss = toByteString1 ben


toByteString1 ::BEncode -> BC.ByteString
toByteString1 (BInt i)   = B.concat $ map BC.pack ["i", show i, "e"]
toByteString1 (BStr  bs) = B.concat [BC.pack(show (B.length bs) ++":"), bs]
toByteString1 (BList ls) = B.concat [BC.pack "l", BC.concat (map toByteString ls), BC.pack "e"]
toByteString1 (BDic dic) = B.concat [BC.pack "d", dBS dic,BC.pack "e"]
    where
        dBS dic = Map.foldlWithKey' entryToBS BC.empty dic
        entryToBS acc k v = B.concat [acc, toByteString k ,toByteString v]
