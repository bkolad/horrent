{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Main where

import Types
import qualified System.Directory as Dir
import qualified Data.List as L
import qualified Peers.PeersControler as PC
import qualified Tracker.Connector as CN (makePeers)
import qualified Control.Monad as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified FileManager.FileSplitter as FS
import qualified Data.Text as T
import Logger.BasicLogger
import Control.Monad.IO.Class

import Control.Monad.Except
--import





torrentDir = "/Users/blaze/Torrent/Downloads/"

torrent = "/Users/blaze/Torrent/TorrentFiles/MOS2.torrent"
ubuntu = "/Users/blaze/Torrent/TorrentFiles/ub222.torrent"



instance MonadError e m => MonadError e (LoggerT BasicLogger m) where
    throwError = lift . throwError
    catchError = undefined--Identity.liftCatch catchError
{-}
logging :: ( MonadLogger m
           , MonadIO m
           , MonadError String m) => m  ([P.Peer], TP.SizeInfo, B.ByteString, [TP.FileInfo])--}
logging = do
    result <- (CN.makePeers ubuntu)
    return result

--kk :: ( MonadError String m, MonadIO m) => m()
kk = do
    bl <-  start
    runExceptT $ runLogger logging bl
    print "END"

    {--case result of
        Left str -> logMessage $ T.pack str
        Right (peers, sizeInfo, torrentName, fInfos) -> do
            let parentDir    = torrentDir ++  ubuntu
                downloadsDir = parentDir ++ "/Parts/"
                filesDir     = parentDir ++ "/Files/"
            return ()--}

{--
            log $ "Peers " ++  (show (length peers))

            liftIO $ Dir.createDirectoryIfMissing True downloadsDir
            liftIO $ Dir.createDirectoryIfMissing True filesDir

            (problems, missing) <- liftIO $ PC.start peers sizeInfo downloadsDir undefined

            log $  (show missing)
            case missing of
            --    [] ->
            --        liftIO $ FS.concatFiles fInfos downloadsDir filesDir
                ms -> do
                    log $ show problems
                    log $ show ms
--}


{--
main :: IO()
main = do
    bl <- start
    runLogger logging bl--}
