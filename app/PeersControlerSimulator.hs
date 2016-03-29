{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module  PeersControlerSimulator where

import qualified InterpretST as IPST

import Control.Monad.State.Strict
import Control.Monad.Reader.Class
import Control.Monad.Reader

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Message as M
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified TubeDSL as TDSL
import qualified Types as TP
import qualified Peer as P
import qualified Data.Map as Map
import qualified Data.List as L
import Debug.Trace


import System.IO.Unsafe



pSize = 49152

file = BC.pack (concat $ map show [0.. 100000])

env = (splitBs 0 Map.empty file)

infoSize = TP.getSizeData (BC.length file) pSize

test :: MonadReader (Map.Map Int B.ByteString) m
     => m ([(String, B.ByteString)], IPST.ActionST)
test = runStateT testTube startState

--test2 :: (Monad m) => m ((), ActionST)
test2 =  (runReader test env)

cont = L.foldl' (\acc (x, b) -> B.append acc b) B.empty (fst test2)

vals = BC.unpack cont

kk = P.hashFor cont
rr = P.hashFor file

check = cont == file


initPeer = P.Peer { P.hostName  = "Some NAme"
                  , P.port = 22
                  , P.pieces = []
                  , P.infoHash = error "no info hash"
                  , P.unChoked = False
                  , P.buffer = B.empty
                  , P.pieceHashes  = error "no p hashes"
                  , P.sizeInfo = infoSize
                  }

startState = IPST.ActionST { IPST.logS = []
                           , IPST.reqNextLog = []
                           , IPST.sendInterested = False
                           , IPST.sendReqLog = Nothing
                           , IPST.setStatus = []
                           }



fileSink ::  (MonadReader (Map.Map Int B.ByteString) m
             ,MonadState IPST.ActionST m)
         =>  Sink (String, BC.ByteString) m ()
fileSink  = do
    x <- await
    case x of
        Nothing -> return ()
        Just s -> fileSink


testTube :: (MonadReader (Map.Map Int B.ByteString) m
            ,MonadState IPST.ActionST m)
         =>  m [(String, B.ByteString)]
testTube = networkSource
     $= (transPipe IPST.interpret (TDSL.recMessage initPeer))
     $$ CL.consume


networkSource :: (MonadReader (Map.Map Int B.ByteString) m
              ,MonadState IPST.ActionST m)
           =>  Source m M.Message
networkSource = do
    st <- get
    env <- ask
    let gotBf = IPST.sendInterested st
    if gotBf then
        sendNetworkResp
    else
        do yield $ M.Bitfield $ P.pieceLsToBS (Map.keys env)
     --       yield $ M.Have (ecodeOnePeiece 55)
           let msg = trace "UYYYYYY" M.UnChoke
           yield msg
           networkSource


sendNetworkResp :: (MonadReader (Map.Map Int B.ByteString) m
                ,MonadState IPST.ActionST m)
             =>  Source m M.Message
sendNetworkResp = do
    st <- get
    case IPST.sendReqLog st of
        Nothing -> return ()
        Just r@(i, off, size) -> do
            env <- ask
            let bs = getChunk env r
            let k = trace ("!!!!!!!!!!!!!!!! "++(show i ++" "++show off)) i
            yield $ M.Piece (k, off, bs)
            sendNetworkResp


getChunk ::  Map.Map Int B.ByteString -> (Int, Int, Int) -> B.ByteString
getChunk m (index, offset, size) =
    let mbs = Map.lookup index m
    in case mbs of
         Nothing -> error  "Impossible"
         Just bs -> B.take size (B.drop offset bs)


splitBs :: Int
     -> Map.Map Int B.ByteString
     -> B.ByteString
     -> Map.Map Int B.ByteString
splitBs acc m bs =
    let (s, cont) = B.splitAt pSize bs
        newMap = Map.insert acc s m
    in  if B.null cont then
            newMap
        else
            splitBs (acc+1) newMap cont
