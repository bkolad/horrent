{-# LANGUAGE NoMonomorphismRestriction, ConstraintKinds, ScopedTypeVariables, FlexibleContexts #-}

module Tracker.HTTPTracker where

import qualified Tracker.UrlEncoder as Encoder (urlEncodeVars)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Peers.Handshake as H
import qualified Bencode.BInfo as BI
import qualified Network as N
import qualified Types as TP

import Tracker.TrackerUtils (getIPandPort)
import Network.HTTP.Base (urlEncodeVars)
import Network.HTTP as HTTP
import Data.Binary.Get
import Utils (io2ExceptT)
import Control.Monad.IO.Class
import Control.Monad.Except
import Logger.BasicLogger




getHostsAndIps :: ( MonadLogger m l
                  , MonadIO m
                  , MonadError String m)
               => BC.ByteString
               -> String
               ->  BI.BEncode
               -> m [(N.HostName, N.PortNumber)]
getHostsAndIps tracker infoH torrentContent = do
    url         <- TP.tryEither $ trackerUrl tracker infoH torrentContent
    rsp         <- liftIO $ getResponseFromTracker url
    parsedResp  <- TP.tryEither $ BI.parse2BEncode . BC.pack $ rsp
    let peersBS = BI.peers parsedResp
    case peersBS of
        Right x ->
            return $ getIPandPort x
        Left msg ->
            TP.throwError (msg ++ " URL: " ++ url ++ " RSP: " ++rsp ++" "++(show tracker))



getResponseFromTracker :: String -> IO String
getResponseFromTracker url =
    HTTP.simpleHTTP (HTTP.getRequest url)
    >>= HTTP.getResponseBody


trackerUrl :: BC.ByteString -> String ->  BI.BEncode -> Either String String
trackerUrl ann infoHash romDic = do
    let vars = encodedVars infoHash
    return $ BC.unpack ann ++ "?" ++ vars
   where
        encodedVars hash =
            Encoder.urlEncodeVars [("info_hash", hash),
                                   ("peer_id", H.myId),
                                   ("left", "1000000000"),
                                   ("port", "6881"),
                                   ("compact", "1"),
                                   ("uploaded", "0"),
                                   ("downloaded", "0"),
                                   ("event", "started")]
