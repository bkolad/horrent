
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module FileManager.FileSplitter (concatFiles) where



import Data.Conduit
import Control.Monad.Trans (lift)
import Data.Foldable (toList)
import Data.Conduit.Filesystem (sourceDirectory)
import qualified Data.Conduit.Binary as CB
import qualified Types as TP
import qualified Data.ByteString as B
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Conduit.List as CL
import Data.List as L
import qualified System.Directory as Dir
import qualified Data.ByteString.Char8 as BC
import qualified Data.Foldable as F



concatFiles :: [TP.FileInfo] -> String -> String -> IO ()
concatFiles fInfos downloadsDir filesDir = do
    fs <- Dir.getDirectoryContents downloadsDir


    let sourceF = sourcesFolder fs
        sinksFiles = createSinks fInfos downloadsDir sinkP

    runResourceT $ connectSourceandSinks sourceF sinksFiles
                 >>= closeResumableSource

    where
        sourcesFolder fs =
            newResumableSource $ makeSource (downloadsDir, fs)




createSinks fInfos downloadsDir sink =
    fmap (\(TP.FileInfo fn sz) ->
        sink downloadsDir (BC.unpack fn) sz) fInfos


connectSourceandSinks = F.foldlM app
    where
        app rs sk =
            fst <$> (rs $$++ sk)



sL :: MonadIO m => Source m Int
sL = sourceList [1 .. 15]--[1,2,3,4,5,6,8,9,0]

test :: IO ()
test = do

    let ff = [TP.FileInfo (BC.pack "aaa") 9, TP.FileInfo (BC.pack "xxx") 20]

        sinkF = createSinks ff "" sinkP2

    runResourceT $ connectSourceandSinks (newResumableSource sL) sinkF
                    >>= closeResumableSource



makeSource :: MonadResource m
           => (String, [String])
           -> Source m B.ByteString
makeSource (downloadsDir, files) =
    let filteredFs = L.filter (\f -> L.isSuffixOf ".part" f) files
        sorted = L.sortOn toInt filteredFs
        allF = fmap (downloadsDir  ++ ) sorted
     in fileSource allF


makeSink  :: MonadResource m
          => String
          -> String
          -> Int
          -> Sink B.ByteString m ()
makeSink dir title i
    = CB.isolate i
    =$=  CB.sinkFile (dir ++ title)


sinkP  :: MonadResource m
       => String
       -> String
       -> Int
       -> Sink B.ByteString m ()
sinkP dir title i =
    CB.isolate i =$= do
        xM <- await
        case xM of
            Nothing ->
                return ()
            Just x -> do
                liftIO $ print (title, x)
                sinkP dir title i


sinkP2  :: MonadResource m
       => String
       -> String
       -> Int
       -> Sink Int m ()
sinkP2 dir title i =
    CL.isolate i =$= do
        xM <- await
        case xM of
            Nothing ->
                return ()
            Just x -> do
                liftIO $ print (title, x)
                sinkP2 dir title i



toInt :: String -> Maybe Integer
toInt st =
    let a =  L.takeWhile ( /= '.') st
        k = reads a :: [(Integer, String)]
    in case k of
        [(i, "")] -> Just i
        _ -> Nothing


fileSource :: MonadResource m => [String] -> Source m B.ByteString
fileSource fNames =
        mergeSources $ fmap CB.sourceFile fNames


mergeSources :: (Foldable f, Monad m) => f (Source m a) -> Source m a
mergeSources = mergeR . fmap newResumableSource . toList


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
