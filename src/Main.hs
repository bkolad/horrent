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




torrentDir = "/Users/blaze/Torrent/Downloads/"

torrent = "/Users/blaze/Torrent/TorrentFiles/MOS2.torrent"
ubuntu = "/Users/blaze/Torrent/TorrentFiles/ub222.torrent"


main :: IO()
main = do
    result <- runExceptT (CN.makePeers ubuntu)
    case result of
        Left str -> print str
        Right (peers, sizeInfo, torrentName, fInfos) -> do
            let parentDir    = torrentDir ++ BC.unpack torrentName
                downloadsDir = parentDir ++ "/Parts/"
                filesDir     = parentDir ++ "/Files/"

            Dir.createDirectoryIfMissing True downloadsDir
            Dir.createDirectoryIfMissing True filesDir
            FS.concatFiles fInfos downloadsDir filesDir
    --        (problems, missing) <- PC.start peers sizeInfo downloadsDir

            return ()
        --    FS.concatFiles fInfos downloadsDir filesDir
{--
            (problems, missing) <- PC.start peers sizeInfo downloadsDir

            case missing of
                [] ->
                    FS.concatFiles fInfos downloadsDir filesDir
                ms -> do
                    print problems
                    print ms


concatFiles :: [FileInfo] -> String -> String -> IO ()
concatFiles fInfos downloadsDir filesDir = undefined


--}
    --    Right x -> process x

{--
process (problems, missing, torrentName) =
    if (null missing)
        then
            do print "conc"
               concatFiles torrentName
               print "OK"
        else
            do print "PROBLEMS"
               print $ show problems
               print $ show missing


concatFiles torrentName =
    do allF <- getDirectoryContents "downloads/"
       let sortedFiles = L.sortOn (read :: String -> Int) $ filter isInteger allF
       M.mapM (app torrentName) sortedFiles



isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False


app torrentName fN =
    do print fN
       cont <- B.readFile ("downloads/"++fN)
       B.appendFile torrentName cont --}
