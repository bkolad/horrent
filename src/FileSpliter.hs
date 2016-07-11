-- https://hackage.haskell.org/package/conduit-merge
-- https://gist.github.com/NicolasT/5436303#file-merge-hs

{-# LANGUAGE RankNTypes #-}

module FileSpliter where

import System.Directory (getDirectoryContents)
import Data.List.Ordered
import qualified Data.List as L
import qualified Data.Conduit.Merge as M
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString as B
import qualified  Data.ByteString.Lazy as LB
import Data.Conduit
import Control.Monad.Trans.Resource (MonadResource, runResourceT)


import Data.Conduit.List (consume, sourceList, sourceNull)
import Data.Conduit.Internal (ConduitM(..), Pipe(..))
import Control.Monad (liftM, liftM2)
import Control.Monad.Trans (lift)
import Data.Foldable (toList)
import qualified Bencode.BInfo as BI
import qualified Types as TP
import qualified Data.ByteString.Char8 as C8


dir = "/Users/blaze/Projects/Haskell/horrent/app/downloads/"
torrent = "/Users/blaze/Projects/Haskell/horrent/app/MOS.torrent"


isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False


fileNames =
   do allF <- getDirectoryContents dir
      let sorted = L.sortOn (read :: String -> Int) $ filter isInteger allF
      return $ map (\x -> dir ++ x) sorted


source :: MonadResource m => [String] -> Source m B.ByteString
source fNames =
        mergeSources $ map CB.sourceFile fNames





sizeInfo = do
    tContent <- BI.parseFromFile torrent
    TP.liftEither $ BI.parsePathAndLenLs tContent

sinkToFile :: MonadResource m => String -> Sink B.ByteString m ()
sinkToFile title = CB.sinkFile (dir ++ title)


sinkPrint :: MonadResource m => Int -> Sink (B.ByteString, Int) m ()
sinkPrint acc = do
    mx <- await
    case mx of
        Nothing -> TP.liftIO $ print $ "boo " ++ (show acc)
        Just (x, i) -> do
            let a = (B.length x)
            TP.liftIO $ print $ (show acc) ++ "   "++(show i)
            sinkPrint (a + acc)

main = do
    mvs <- TP.runExceptT sizeInfo
    case mvs of
        Left l -> print l
        Right ls -> do
            let (title, sz) = head ls
            fNames <- fileNames
            print fNames
            let sc = source fNames
            print sz
            runResourceT $ sc $$ (CB.isolate sz) =$= (sinkToFile (C8.unpack title))



mergeSources :: (Foldable f, Monad m) => f (Source m a) -> Source m a
mergeSources = mergeR . fmap newResumableSource . toList


mergeResumable :: Monad m => Int ->  [ResumableSource m o] -> Source m (o, Int)
mergeResumable i [] = pure ()
mergeResumable i (s : sources) = do
    a <- lift $ s $$+- await
    case a of
        Nothing -> mergeResumable (i + 1) sources
        Just x -> do
            yield (x, i)
            mergeResumable i (s : sources)


mergeR :: Monad m => [ResumableSource m o] -> Source m o
mergeR [] = pure ()
mergeR (s : sources) = do
    (rs, a) <- lift $ s $$++ await
    case a of
        Nothing ->do
             lift $ closeResumableSource s
             mergeR sources
        Just x -> do
            yield x
            mergeR (rs : sources)
