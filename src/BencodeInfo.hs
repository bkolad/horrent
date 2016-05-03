{-# LANGUAGE RankNTypes #-}

module BencodeInfo (BP.BEncode, annouce, infoHash, parseFromFile, parseFromBS,
peers, piceSize, torrentSize, piecesHashSeq, torrentName) where

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
import Control.Monad (join)

import Types
import Debug.Trace
import qualified BencodeParser as BP
import Control.Lens
import qualified Data.Map as Map
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class


data DicInfo = Announce
             | PiecesHash
             | Peers
             | PieceLength
             | Info
             | InfoLength
             | Name
             | MultiFiles
             deriving Show


mkLens :: DicInfo -> Prism' BP.BEncode BP.BEncode
mkLens di =
    case di of
        Announce    -> BP.keyL "announce"
        Peers       -> BP.keyL "peers"
        Info        -> infoLens
        PiecesHash  -> infoLens . BP.keyL "pieces"
        PieceLength -> infoLens . BP.keyL "piece length"
        InfoLength  -> infoLens . BP.keyL "length"
        Name        -> infoLens . BP.keyL "name"
        MultiFiles  -> infoLens . BP.keyL "files"
        where
            infoLens :: Prism' BP.BEncode BP.BEncode
            infoLens = BP.keyL "info"


filesLs :: Prism' BP.BEncode BP.BEncode
filesLs = (BP.keyL "info") . (BP.keyL "files")


genericGet dI lenS dic =
    let ret = dic ^? (mkLens dI) . lenS
        msg = "Bencode parsing error: Missing " ++ (show dI)
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


info :: BP.BEncode -> Either String String
info dic = show <$> (genericGet Info BP.idL dic)


files :: BP.BEncode -> Either String BP.BEncode
files dic = (genericGet MultiFiles BP.idL dic)


torrentName :: BP.BEncode -> Either String BC.ByteString
torrentName = genericGet Name BP.bStrL


splitEvery :: Int -> BC.ByteString -> HashInfo
splitEvery n bc = if (BC.null bc)
                     then Seq.empty
                     else s Seq.<| (splitEvery n e)
                  where (s,e) = BC.splitAt n bc


piecesHashSeq :: BP.BEncode -> Either String HashInfo
piecesHashSeq dic = (splitEvery 20) <$> piecesHash dic


multiFiles :: BP.BEncode -> Either String [([BC.ByteString], Int)]
multiFiles dic = do
    filesBencode <- files dic
    let fs =  (sequence . toPathLen . children) filesBencode
    maybe (Left "Wrong multi-files section in infodic") Right fs


parseFromFile :: String ->  ExceptT String IO BP.BEncode
parseFromFile path = do content <- liftIO $ B.readFile path
                        liftEither $ P.parseOnly BP.bencodeParser content


parseFromBS :: B.ByteString -> Either String BP.BEncode
parseFromBS x = P.parseOnly BP.bencodeParser x

torrent = "Hunger.torrent"


printer:: IO()
printer = do content <- runExceptT $ parseFromFile torrent
             case content of
                  Left l ->
                      print $ "Problem with reading torrent file" ++ (show l)
                  Right dic ->
                      print $ multiFiles dic



toPathLen :: [BP.BEncode] -> [Maybe ([BC.ByteString], Int)]
toPathLen ls =
    let path = BP.keyL "path" . BP.listL . traverse . BP.bStrL
        len  = BP.keyL "length" . BP.bIntL
    in map (\dic -> sequence (dic ^.. path, dic ^? len)) ls
