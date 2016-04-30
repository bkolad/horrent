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
mkLens Name        = (BP.keyL "info") . (BP.keyL "name")

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
