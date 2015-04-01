{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, DoAndIfThenElse #-}

module Connector where

import qualified Peer as P (Peer, makePeer, myId, talk, showPeer) 
import qualified BencodeParser as BP (BEncode, annouce, infoHash, parseFromFile, parseFromBS, peers)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified UrlEncoder as Encoder (urlEncodeVars)
import Data.Binary.Get
import Data.Word
import Data.Either

import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP as HTTP
import Network.Socket
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception as E
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans
import           Data.Char
import Data.Bits  ( (.&.), (.|.), shiftL, shiftR )


import Data.ByteString.Base16 (encode)




type ErrorIO = ErrorT String IO

liftEither = ErrorT . return


                  
getResponseFromTracker :: String -> IO String
getResponseFromTracker url = HTTP.simpleHTTP (HTTP.getRequest url) >>= HTTP.getResponseBody 



trackerUrl :: BP.BEncode -> Either String String
trackerUrl fromDic = do ann <- BP.annouce fromDic
                        vars <- encodedVars <$> (BP.infoHash fromDic)
                        return $ ann++"?"++vars
     where encodedVars hash = Encoder.urlEncodeVars [("info_hash", hash),
                                                     ("peer_id", P.myId),
                                                     ("left", "1000000000"),
                                                     ("port", "6881"),
                                                     ("compact", "1"),
                                                     ("uploaded", "0"),
                                                     ("downloaded", "0"),
                                                     ("event", "started")]         

                 
getIPandPort :: BC.ByteString -> [(Word32, Word16)]                                                                                                        
getIPandPort bs = runGet get32and16b (BL.fromChunks [bs])                                                                                             
       where get32and16b :: Get [(Word32, Word16)]
             get32and16b = do empty <- isEmpty
                              if empty
                              then return [] 
                              else do ip <- getWord32be
                                      port <- getWord16be
                                      rest <- get32and16b
                                      return $ (ip, port):rest



makePeer :: BC.ByteString -> HostName -> PortNumber -> IO (Either String P.Peer)
makePeer hash h p  = E.catch (liftM Right $ P.makePeer hash h p)
                             (\(e::SomeException) -> return . Left $ show e )
                       
  
  
 --showResponse :: ErrorIO [P.Peer]
showResponse = do content <-  liftIO $ BP.parseFromFile "ubuntu.torrent"
                  hash <- liftEither $ BC.pack<$>(content >>= BP.infoHash)
                  url <-   liftEither $ content >>= trackerUrl
                  resp <- (liftIO . getResponseFromTracker) url
                  peersBS <- liftEither $ ((BP.parseFromBS . BC.pack) resp)  >>= BP.peers
                  let ls =  getIPandPort peersBS
                  peers <- liftIO $ mapConcurrently (\(h,p)->  makePeer hash (show h) (fromIntegral p)) (take 5 ls) 
                  let (_, pp) = partitionEithers peers  
                  liftIO $ mapConcurrently P.talk pp
                  liftIO $ mapM P.showPeer pp
              
            --      return $ ls
                  
toIO = runErrorT showResponse


