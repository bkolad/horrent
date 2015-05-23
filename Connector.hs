{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, DoAndIfThenElse #-}

module Connector ( liftEither, makePeers) where

import qualified Peer as P (Peer, makePeer, myId, showPeer) 
import qualified BencodeParser as BP (BEncode, annouce, infoHash, parseFromFile, parseFromBS, peers, piceSize, torrentSize)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified UrlEncoder as Encoder (urlEncodeVars)
import Data.Binary.Get
import Data.Word
import qualified Data.Either as DE 
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP as HTTP
import qualified Network as N
import Control.Concurrent.Async as Async (mapConcurrently)
import Control.Exception as E
import Control.Applicative
import Control.Monad.Error
import Data.ByteString.Base16 (encode)
import Types


liftEither = ErrorT . return

                      
makePeers :: String ->Int -> ErrorT String IO [P.Peer]  
makePeers tracker numberOfP = do content <-  liftIO $ BP.parseFromFile tracker
                                 hash <- liftEither $ BC.pack<$>(content >>= BP.infoHash)
                                 url <-   liftEither $ content >>= trackerUrl
                                 pSize<- liftEither $ content >>= BP.piceSize
                                 tSize<- liftEither $ content >>= BP.torrentSize
                                 let pNum= ceiling $ ((fromIntegral tSize)/(fromIntegral pSize)) -- +5
                                 liftIO $ print pNum    
                                 liftIO $ print tSize
                                 liftIO $ print pSize
                                 resp <- (liftIO . getResponseFromTracker) url
                                 peersBS <- liftEither $ ((BP.parseFromBS . BC.pack) resp)  >>= BP.peers
                                 let ls =  getIPandPort peersBS    
                                 gpi <- liftIO $ newGlobalBitField pNum
                                 peers <- liftIO $ Async.mapConcurrently (\(h,p)->  makePeer hash (show h) (fromIntegral p) pNum gpi) (take numberOfP ls) 
                                 let (ll, pp) = DE.partitionEithers peers
                                 case ll of
                                      [] -> return []
                                      _ -> liftIO $ (:)<$>(print "ERROR ") <*> (mapM print ll)   
                                 return pp
                                 

makePeer :: BC.ByteString -> N.HostName -> N.PortNumber -> Int -> GlobalPiceInfo -> IO (Either String P.Peer)
makePeer hash h p size gpi = E.catch (liftM Right $ P.makePeer hash h p size gpi)
                                     (\(e::SomeException) -> return . Left $ (show e) ++" "++ (show h))                                 

                             
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
                             

