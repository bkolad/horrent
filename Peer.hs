{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse, FlexibleInstances, UndecidableInstances #-}
module Peer (Peer, makePeer, myId, showPeer, intFromBS, peerP, handleP, amIInterested, amIChocked, amIVirgin, bitField) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import qualified Network as N
import qualified System.IO as SIO
import Data.IORef


myId = "-TR2840-d0p22uiake0b" 
protocol = "BitTorrent protocol"


 
data Peer = Peer{ handleP :: SIO.Handle
                , peerP :: String 
                , amIInterested :: IORef Bool -- false
                , amIChocked :: IORef Bool  -- true
                , amIVirgin :: IORef Bool -- first time I am talking to a peer
                , bitField :: IORef B.ByteString
                } 
                       

showPeer :: Peer -> IO (String, B.ByteString)                       
showPeer p= do let name = peerP p
               bf <- readIORef $ bitField p    
               return (name, bf)
                         

                 
makePeer :: BC.ByteString -> N.HostName -> N.PortNumber -> IO Peer             
makePeer hash host port = do handle<- N.connectTo host (N.PortNumber  port)
                             SIO.hSetBuffering handle SIO.NoBuffering                            
                          --   hSetBuffering handle (BlockBuffering Nothing)
                             --hSetBuffering handle LineBuffering
                             sendHandshake handle hash $ BC.pack myId
                             recvHandshake handle                                         
                               
             
sendHandshake :: SIO.Handle -> B.ByteString -> B.ByteString -> IO ()
sendHandshake handle hash peer = BC.hPutStr handle msg -- >> print "Handshake finished"
        where msg = B.concat [len, ptr, rsrv, hash, peer]
              len = B.singleton $ (fromIntegral . length) protocol
              ptr = BC.pack protocol
              rsrv = B.replicate 8 (fromIntegral 0)       
              
              
recvHandshake :: SIO.Handle -> IO Peer
recvHandshake handle = do 
                          len <- B.hGet handle 1
                          ptr <- B.hGet handle $ intFromBS len
                          rsrv <- B.hGet handle 8
                          hash <- B.hGet handle 20
                          peer <- B.hGet handle 20 
                          amIVirgin <- newIORef True
                          amIChocked <- newIORef True
                          amIInterested <- newIORef False
                          byteField <-  newIORef B.empty                   
                          return $ Peer handle (BC.unpack peer) amIInterested amIChocked amIVirgin byteField

                          
intFromBS :: BC.ByteString -> Int  
intFromBS = fromIntegral . head . B.unpack


                    
                     
                                                                        



                            
                            
               
