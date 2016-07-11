module Main where

import Types
import System.Directory
import Data.List.Ordered
import qualified Data.List as L
import qualified PeersControlerN as PC
import qualified Control.Monad as M
import qualified Data.ByteString as B

torrent = "/Users/blaze/Projects/Haskell/horrent/app/MOS2.torrent"
ubuntu = "/Users/blaze/Projects/Haskell/horrent/app/ub222.torrent"


main::IO()
main = do result <- runExceptT $ PC.startM torrent
          case  result of
              Left str -> print str
              Right x -> process x


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
       B.appendFile torrentName cont
