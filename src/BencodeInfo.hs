module BencodeInfo (BP.BEncode, annouce, infoHash, parseFromFile, parseFromBS,
peers, piceSize, torrentSize, piecesHashSeq) where

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
import qualified BencodeParser as BP
import Control.Lens


-- http://blog.sumtypeofway.com/recursion-schemes-part-2/


data DicInfo = Announce
             | PiecesHash
             | Peers
             | PieceLength
             | Info
             | InfoLength
             | Name
             deriving Show

mkLens Announce    = BP.keyL "announce"
mkLens PiecesHash  = (BP.keyL "info") . (BP.keyL "pieces")
mkLens Peers       = (BP.keyL "peers")
mkLens PieceLength = (BP.keyL "info") . (BP.keyL "piece length")
mkLens Info        = BP.keyL "info"
mkLens InfoLength  = (BP.keyL "info") . (BP.keyL "length")
mkLens Name        = (BP.keyL "info") .(BP.keyL "name")

genericGet dI lenS dic =
    let ret = dic ^? (mkLens dI) . lenS
        msg = "Bencode parsing error: Missing " ++ (show dI) ++" "++(show dic)
    in maybe (Left msg) Right ret


annouce :: BP.BEncode -> Either String String
annouce dic = BC.unpack <$> (genericGet Announce BP.bStrL dic)


peers :: BP.BEncode -> Either String BC.ByteString
peers = genericGet Peers BP.bStrL


piecesHash :: BP.BEncode -> Either String BC.ByteString
piecesHash = genericGet PiecesHash BP.bStrL


piceSize :: BP.BEncode -> Either String Int
piceSize = genericGet PieceLength BP.bIntL


torrentSize :: BP.BEncode -> Either String Int
torrentSize = genericGet InfoLength BP.bIntL


infoHash :: BP.BEncode -> Either String String
infoHash dic = fun <$> (genericGet Info BP.idL dic)
    where
        fun = BC.unpack . SHA1.hash . BP.toByteString


torrentName :: BP.BEncode -> Either String BC.ByteString
torrentName = genericGet Name BP.bStrL



splitEvery :: Int -> BC.ByteString -> HashInfo
splitEvery n bc = if (BC.null bc)
                     then Seq.empty
                     else s Seq.<| (splitEvery n e)
                  where (s,e) = BC.splitAt n bc


piecesHashSeq :: BP.BEncode -> Either String HashInfo
piecesHashSeq dic = (splitEvery 20) <$> piecesHash dic


parseFromFile :: String ->  ExceptT String IO BP.BEncode
parseFromFile path = do content <- liftIO $ B.readFile path
                        liftEither $ P.parseOnly BP.bencodeParser content


parseFromBS :: B.ByteString -> Either String BP.BEncode
parseFromBS x = P.parseOnly BP.bencodeParser x

torrent = "ub222.torrent"

printer:: IO()
printer = do content <- runExceptT $ parseFromFile torrent
             case content of
                  Left l ->
                      print $ "Problem with reading torrent file" ++ (show l)
                  Right dic -> do
                      print $ annouce dic
                      print $ torrentName dic
                      print $ piceSize dic
                      print $ torrentSize dic
                      print $ infoHash dic



{--PRIVATE--}
{--
find :: String -> BEncode -> Either String  BEncode
find keyS (BDic ls) = mHead $ filtered ls
   where filtered l = filter (\(k,v)-> k == key) l
         key =  BC.pack keyS
         mHead [] = Left $ "Couldn't find "++keyS++" key"
         mHead ((x1,x2):xs) = Right x2
find keyS _ = Left "Give me dictionary!"


toByteString::BEncode->BC.ByteString
toByteString (BInt i) = B.concat $ map BC.pack ["i", show i, "e"]
toByteString (BStr  bs) = B.concat [BC.pack(show (B.length bs) ++":"), bs]
toByteString (BList ls) = BC.concat [BC.pack "l", BC.concat (map toByteString ls), BC.pack "e"]
toByteString (BDic [])=BC.pack ""
toByteString (BDic ls) = B.concat [BC.pack "d",entryToBS ls ,BC.pack "e"]
  where entryToBS [] = BC.pack ""
        entryToBS ((k,v):xs)= B.concat [toByteString $ BStr k ,toByteString v, entryToBS xs]




{--TESTS--}

torrent = "ub222.torrent"--"karl_marx.torrent"--"tom.torrent"--"tom.torrent" -- "karl_marx.torrent"--"tom.torrent" --

printer:: IO()
printer = do content <- runExceptT $ parseFromFile torrent
             case content of
                  Left l -> print $ "Problem with reading torrent file" ++ (show l)
                  Right dic -> putStr $ prettyPrint 0 dic


infoPrinter::IO()
infoPrinter = do content <- runExceptT $ parseFromFile torrent
                 case content >>= (find "info") of
                      Left e-> print e
                      Right c  -> print (toByteString c)

piecesPrinter::IO()
piecesPrinter = do content <- runExceptT $ parseFromFile torrent
                   case content>>=piecesHashSeq of
                      Left e-> print e
                      Right c  -> print $ B16.encode (Seq.index c 10)


emptySpace n =  concat $ replicate n "  "


nl = ("\n")

prettyPrint :: Int -> BEncode -> String
prettyPrint n (BInt i) = show i
prettyPrint n (BStr (s)) = show s
prettyPrint n (BList []) = ""
prettyPrint n (BList (x:xs)) = nl ++ (emptySpace (n+3))
                                   ++ (prettyPrint (n+1) x)
                                   ++ (prettyPrint n (BList xs))
prettyPrint n (BDic []) = ""
prettyPrint n (BDic ((key, value):xs)) = nl ++ (emptySpace n)
                                                     ++ (show key) ++ " ::-> "
                                                     ++ (prettyPrint (n+1) value)
                                                     ++ (prettyPrint n (BDic xs))
--}
