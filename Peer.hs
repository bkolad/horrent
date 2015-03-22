{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse #-}
module Peer (Peer, makePeer, myId, talk) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Control.Exception as E
import Data.Tuple
import Data.Binary.Get
import Network
import System.IO
import Data.Binary.Put
import Control.Applicative
import Control.Concurrent


myId = "-TR2840-d0p22uiake0b" 
protocol = "BitTorrent protocol"


data Message = KeepAlive 
             | Choke
             | UnChoke
             | Interested
             | NotInterested
             | Have
             | Bitfield
             | Request
             | Piece
             | Cancel
             | Port 
             | Error
             deriving (Show)
             
data MessageId = NoId | Id Int             


msgToLenAndId msg = case msg of
                         KeepAlive     -> (0, NoId)
                         Choke         -> (1, Id 0)
                         UnChoke       -> (1, Id 1)
                         Interested    -> (1, Id 2)
                         NotInterested -> (1, Id 3)
                         Have          -> (5, Id 4)
                         Bitfield      -> undefined
                         Request       -> (13, Id 6)
                         Piece         -> undefined
                         Cancel        -> (13, Id 8)
                         Port          -> (3, Id 9)
                        
lenAndIdToMsg lenId = case lenId of
                          (0, NoId)   -> KeepAlive
                          (1, Id 0)   -> Choke
                          (1, Id 1)   -> UnChoke
                          (1, Id 2)   -> Interested
                          (1, Id 3)   -> NotInterested
                          (5, Id 4)   -> Have
                          (len, Id 5) -> Bitfield
                          (13, Id 6)  -> Request
                          (len, Id 7) -> Piece 
                          (13, Id 8)  -> Cancel
                          (3, Id 9)   -> Port
                        
                    
lenAndIdToMsg_M lenId_M = lenAndIdToMsg <$> lenId_M              
             
data Peer = Peer{ handleP :: Handle
                , peerP :: String 
                , amIInterested :: Bool
                , theyInterested :: Bool
                , amIChocked :: Bool
                , theyChocked :: Bool
                } deriving (Show)
                             

talk :: Peer -> IO ()
talk peer =  E.catch (talkToPeer peer) (\(e::E.SomeException) -> print $ "Failure "++(peerP peer) ++(show e) {-- TODO close the connection--} )


talkToPeer :: Peer -> IO ()
talkToPeer peer = do let handle = handleP peer
                     lenAndId <- getMessage handle
                     let msg = lenAndIdToMsg_M lenAndId
                     case msg of
                          Just KeepAlive -> loopAndWait peer "Alive"
                          Just UnChoke   -> print "UnCHocked"
                          Just Bitfield  -> (sendMsg handle Interested) >> loopAndWait peer "BBBBBB"
                          _-> loopAndWait peer (show msg)
                          where 
                            loopAndWait peer m  =  (threadDelay 100000) >> (print m)>>talkToPeer peer 
                     

                     
sendMsg :: Handle -> Message -> IO ()
sendMsg handle msg = case (msgToLenAndId msg) of
                          (x, NoId) -> send $ putWord32be x
                          (x, Id y) -> send $ putWord32be x >> putWord8 (fromIntegral y)
                          where 
                            send = BL.hPutStr handle . runPut  
          
          
getMessage :: Handle -> IO (Maybe (Int, MessageId))
getMessage handle = do numBytes <- BL.hGet handle 4
                       if (BL.length numBytes) < 4 then (threadDelay 10000000)>> (hFlush handle) >> getMessage handle 
                       else 
                          do let sizeOfTheBody = (readBEInt numBytes)
                             case sizeOfTheBody of
                                  0 -> return $ Just (0, NoId) 
                                  otherwise -> --(Just . swap)<$>(getMsg handle sizeOfTheBody) 
                                              do 
                                                 (msgId, body) <- getMsg handle sizeOfTheBody 
                                                 return $ Just  (sizeOfTheBody, msgId)
                             where  
                               getMsg handle size = (,)<$>(Id . intFromBS <$> B.hGet handle 1)<*> (B.hGet handle (size -1))
                               readBEInt = fromIntegral  . runGet getWord32be                            


                 
makePeer :: BC.ByteString -> HostName -> PortNumber -> IO Peer             
makePeer hash host port = do handle<- connectTo host (PortNumber  port)
                             hSetBuffering handle NoBuffering                            
                          --   hSetBuffering handle (BlockBuffering Nothing)
                             --hSetBuffering handle LineBuffering
                             sendHandshake handle hash $ BC.pack myId
                             recvHandshake handle                                         
                               
             
sendHandshake :: Handle -> B.ByteString -> B.ByteString -> IO ()
sendHandshake handle hash peer = BC.hPutStr handle msg -- >> print "Handshake finished"
        where msg = B.concat [len, ptr, rsrv, hash, peer]
              len = B.singleton $ (fromIntegral . length) protocol
              ptr = BC.pack protocol
              rsrv = B.replicate 8 (fromIntegral 0)       
              
              
recvHandshake :: Handle -> IO Peer
recvHandshake handle = do 
                          len <- B.hGet handle 1
                          ptr <- B.hGet handle $ intFromBS len
                          rsrv <- B.hGet handle 8
                          hash <- B.hGet handle 20
                          peer <- B.hGet handle 20 
                          return $ Peer handle (BC.unpack peer) True False True True

                          
intFromBS :: BC.ByteString -> Int  
intFromBS = fromIntegral . head . B.unpack


                    
                     
                                                                        



                            
                            
               
