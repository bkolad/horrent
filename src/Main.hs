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




torrentDir = "/Users/blaze/Torrent/Downloads/"

torrent = "/Users/blaze/Torrent/TorrentFiles/MOS2.torrent"
ubuntu = "/Users/blaze/Torrent/TorrentFiles/ub222.torrent"

main = do
    logger <- startLogger
    let lpee = CN.makePeers ubuntu
    result <- runExceptT $ runLogger lpee logger
    case result of
        Left str -> print $ T.pack str
        Right (peers, sizeInfo, torrentName, fInfos) -> do
            let parentDir    = torrentDir ++  ubuntu
                downloadsDir = parentDir ++ "/Parts/"
                filesDir     = parentDir ++ "/Files/"
                controller = PC.start peers sizeInfo downloadsDir
            r <- runExceptT $ runLogger controller logger
            case r of
                Left x -> print x
                Right (problems, missing) -> do
                    case missing of
                        [] -> FS.concatFiles fInfos downloadsDir filesDir
                        ms -> do
                            print $ show problems
                            print $ show ms
