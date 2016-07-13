{-# LANGUAGE RankNTypes #-}

module Bencode.BInfo
    ( BP.BEncode
    , AnnounceType(..)
    , annouce
    , infoHash
    , parseFromFile
    , BP.parse2BEncode
    , peers
    , piceSize
    , torrentSize
    , piecesHashSeq
    , torrentName
    , parsePathAndLenLs
    , makeSizeInfo
    , getAnnounce
    , BP.parseUDPAnnounce) where


import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Crypto.Hash.SHA1 as SHA1 (hash)
import qualified Data.Sequence as Seq
import qualified Bencode.BParser as BP
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Control.Monad (join)

import Types
import Control.Lens


data DicInfo = Announce
             | PiecesHash
             | Peers
             | PieceSize
             | Info
             | SingleFile
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
        PieceSize   -> infoLens . BP.keyL "piece length"
        SingleFile  -> infoLens . BP.keyL "length"
        Name        -> infoLens . BP.keyL "name"
        MultiFiles  -> infoLens . BP.keyL "files"
        where
            infoLens :: Prism' BP.BEncode BP.BEncode
            infoLens = BP.keyL "info"


filesLs :: Prism' BP.BEncode BP.BEncode
filesLs = BP.keyL "info" . BP.keyL "files"


genericGet dI lenS dic =
    let ret = dic ^? (mkLens dI) . lenS
        msg = "Bencode parsing error: Missing " ++ show dI
    in maybe (Left msg) Right ret


isSingleFile :: BP.BEncode -> Bool
isSingleFile dic =
    let ret = dic ^? (mkLens SingleFile) . BP.bIntL
    in isJust ret--maybe False (const True) ret

annouce :: BP.BEncode -> Either String BC.ByteString--String
annouce = genericGet Announce BP.bStrL


peers :: BP.BEncode -> Either String BC.ByteString
peers = genericGet Peers BP.bStrL


piecesHash :: BP.BEncode -> Either String BC.ByteString
piecesHash = genericGet PiecesHash BP.bStrL


piceSize :: BP.BEncode -> Either String Int
piceSize = genericGet PieceSize BP.bIntL


torrentSize :: BP.BEncode -> Either String Int
torrentSize = genericGet SingleFile BP.bIntL


infoHash :: BP.BEncode -> Either String String
infoHash dic = fun <$> genericGet Info BP.idL dic
    where
        fun = BC.unpack . SHA1.hash . BP.bencode2ByteString


info :: BP.BEncode -> Either String String
info dic = show <$> genericGet Info BP.idL dic


files :: BP.BEncode -> Either String BP.BEncode
files = genericGet MultiFiles BP.idL


torrentName :: BP.BEncode -> Either String BC.ByteString
torrentName = genericGet Name BP.bStrL


splitEvery :: Int -> BC.ByteString -> HashInfo
splitEvery n bc = if BC.null bc
                     then Seq.empty
                     else s Seq.<| splitEvery n e
                  where (s,e) = BC.splitAt n bc


piecesHashSeq :: BP.BEncode -> Either String HashInfo
piecesHashSeq dic = splitEvery 20 <$> piecesHash dic


multiFiles :: BP.BEncode -> Either String [(BC.ByteString, Int)]
multiFiles dic = do
    filesBencode <- files dic
    let fs =   (sequence . lsWordToPath. toPathLen . children) filesBencode
    maybe (Left "Wrong multi-files section in infodic") Right fs


parseFromFile :: String ->  ExceptT String IO BP.BEncode
parseFromFile path = do content <- liftIO $ B.readFile path
                        liftEither $ BP.parse2BEncode content


data AnnounceType = HTTP  BC.ByteString | UDP  BC.ByteString

{--
getAnnounce :: BC.ByteString -> Either String AnnounceType
getAnnounce st =
    if (BC.isPrefixOf (BC.pack("http")) st)
        then return $ HTTP st
        else if (BC.isPrefixOf (BC.pack "udp") st)
            then return $ UDP st
            else Left "Announce type not recognized"
--}

getAnnounce :: BC.ByteString -> Either String AnnounceType
getAnnounce st
    | BC.isPrefixOf (BC.pack "http") st =
        return $ HTTP st
    | BC.isPrefixOf (BC.pack "udp") st =
        return $ UDP st
    | otherwise =
        Left "Announce type not recognized"


makeSizeInfo :: [(BC.ByteString, Int)]
             -> Int -> SizeInfo
makeSizeInfo pNLls pSize =
       let torrentSize = foldl (\acc (_, x) -> x + acc ) 0 pNLls
           numberOfPieces =
               ceiling $ (fromIntegral torrentSize) / (fromIntegral pSize)
           lps = torrentSize `mod` pSize
           lastPieceSize = if lps == 0 then pSize else lps
        in
           SizeInfo numberOfPieces pSize lastPieceSize


parsePathAndLenLs :: BP.BEncode
                  -> Either String [(B.ByteString, Int)]
parsePathAndLenLs content =
        if isSingleFile content
            then
                do tName <- torrentName content
                   tSize <- torrentSize content
                   return [(tName, tSize)]
            else
                multiFiles content


lsWordToPath ::  [Maybe ([BC.ByteString], Int)]
             ->  [Maybe (BC.ByteString, Int)]
lsWordToPath mLs = fmap (fmap concatPath) mLs
    where
        concatPath (ls, i) = (B.concat ls, i)


toPathLen :: [BP.BEncode] -> [Maybe ([BC.ByteString], Int)]
toPathLen ls =
    let path = BP.keyL "path" . BP.listL . traverse . BP.bStrL
        len  = BP.keyL "length" . BP.bIntL
    in map (\dic -> sequence (dic ^.. path, dic ^? len)) ls
