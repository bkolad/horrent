{-# LANGUAGE OverloadedStrings #-}
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
import qualified Logger.BasicLogger as Logger
import qualified Data.Text as T




torrentDir = "/Users/blaze/Torrent/Downloads/"

torrent = "/Users/blaze/Torrent/TorrentFiles/MOS2.torrent"
ubuntu = "/Users/blaze/Torrent/TorrentFiles/ub222.torrent"


main :: IO()
main = do
    l <- Logger.start
    let log = Logger.logString l
    result <- runExceptT (CN.makePeers torrent log)
    case result of
        Left str -> log str
        Right (peers, sizeInfo, torrentName, fInfos) -> do
            let parentDir    = torrentDir ++  ubuntu
                downloadsDir = parentDir ++ "/Parts/"
                filesDir     = parentDir ++ "/Files/"

            log $ "Peers " ++  (show (length peers))

            Dir.createDirectoryIfMissing True downloadsDir
            Dir.createDirectoryIfMissing True filesDir

            (problems, missing) <- PC.start peers sizeInfo downloadsDir log

            log $  (show missing)
            case missing of
                [] ->
                    FS.concatFiles fInfos downloadsDir filesDir
                ms -> do
                    log $ show problems
                    log $ show ms
