{-# LANGUAGE ScopedTypeVariables #-}
module Main where
{-- import PeersControler (start)
import Peer (showPeer) --}
import Types
import System.Directory
import qualified Data.List as L
import Data.List.Ordered
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Control.Monad as M
import Control.Applicative


isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False


app fN= do print fN
           cont <- B.readFile ("downloads/"++fN)
           B.appendFile "cont.xx" cont



files = do allF <- getDirectoryContents "downloads/"
           let sortedFiles = sortOn (read :: String -> Int) $ filter isInteger allF
           print sortedFiles
           print $ ((map show [0.. 2247]) L.\\ sortedFiles)
    --       M.mapM app sortedFiles

main = app "ub.torrent"
