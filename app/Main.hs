{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import PeersControler (start)
import Peer (showPeer)
import Control.Monad.Except
import System.Directory
import qualified Data.List as L
import Data.List.Ordered
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Control.Monad as M
import Control.Applicative

main::IO() 
main = do result <-runExceptT $ start "tom.torrent" 20 --"karl_marx.torrent" 1
          case result of
               Left s -> print s
               Right ps -> do ls<-mapM showPeer ps
                              print ls
                              
                      
      
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False      
      

app fN= do print fN
           cont <- B.readFile ("downloads/"++fN)       
           B.appendFile "fil.jpg" cont
      
      
files = do allF <- getDirectoryContents "downloads/"
           let sortedFiles = sortOn (read :: String -> Int) $ filter isInteger allF
           print $ ((map show [0.. 78]) L.\\ sortedFiles) 
           M.mapM app sortedFiles
           
