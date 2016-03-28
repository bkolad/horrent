{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module InterpretST where

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


import Action
import System.IO.Unsafe


data ActionST = ActionST { logS :: [String]
                         , reqNextLog :: [Int]
                         , sendInterested :: Bool
                         , sendReqLog :: Maybe (Int,Int,Int)
                         , setStatus :: [(Int, TP.PiceInfo)]
                         } deriving Show


{--
type T m a = (MonadReader (Map.Map Int B.ByteString) m, MonadState ActionST m)
          => m a

type Kk m = T (Sink (String, BC.ByteString) m) Int --}


interpret :: (MonadReader (Map.Map Int B.ByteString) m
             ,MonadState ActionST m)
          =>  Action a -> m a
interpret program =
    case program of
        Free (SendInterested c) -> do
            modify (\st -> st {sendInterested = True})
            interpret c


        Free (Log str c) -> do
            modify (\st -> st {logS = str : logS st})
            interpret c


        Free (ReqNextAndUpdate pieces fun) -> do
                st <- get
                let reqLog = reqNextLog st
                    diff = pieces L.\\ reqLog
                case diff of
                    [] -> interpret (fun Nothing)
                    (x:xs) -> do
                        modify (\st -> st {reqNextLog = x : reqNextLog st})
                        interpret $ fun (Just x)


        Free (SendRequest r@(next, offset, chunkSize) c) -> do
            let kk = trace ("RRRRRR "++ (show r)) r
            modify (\st -> st {sendReqLog = Just kk})
            interpret c

        Free (SetStatus x status c) -> interpret c

        Pure x -> return x


startState = ActionST { logS = []
                      , reqNextLog = []
                      , sendInterested = False
                      , sendReqLog = Nothing
                      , setStatus = []
                      }

initPeer = P.Peer { P.hostName  = "Some NAme"
                  , P.port = 22
                  , P.pieces = []
                  , P.infoHash = error "no info hash"
                  , P.globalStatus  = error "no global status"
                  , P.unChoked = False
                  , P.buffer = B.empty
                  , P.pieceHashes  = error "no p hashes"
                  , P.sizeInfo = infoSize
                  }


fileSink ::  (MonadReader (Map.Map Int B.ByteString) m
             ,MonadState ActionST m)
         =>  Sink (String, BC.ByteString) m ()
fileSink  = do
    x <- await
    case x of
        Nothing -> return ()
        Just s -> fileSink


testTube :: (MonadReader (Map.Map Int B.ByteString) m
            ,MonadState ActionST m)
         =>   m [(String, B.ByteString)]
testTube = networkSource
     $= (transPipe interpret (TDSL.recMessage initPeer))
     $$ CL.consume--fileSink



pSize = 49152

file = BC.pack (concat $ map show [0.. 100000])

env = (splitBs 0 Map.empty file)

infoSize = TP.getSizeData (BC.length file) pSize

test :: MonadReader (Map.Map Int B.ByteString) m
     => m ([(String, B.ByteString)], ActionST)
test = runStateT testTube startState

--test2 :: (Monad m) => m ((), ActionST)
test2 =  (runReader test env)

cont = L.foldl' (\acc (x, b) -> B.append acc b) B.empty (fst test2)

vals = BC.unpack cont

kk = P.hashFor cont
rr = P.hashFor file

check = cont == file

networkSource :: (MonadReader (Map.Map Int B.ByteString) m
                 ,MonadState ActionST m)
              =>  Source m M.Message
networkSource = do
    st <- get
    env <- ask
    let gotBf = sendInterested st
    if gotBf then
        sendNetworkResp
    else
        do yield $ M.Bitfield $ P.pieceLsToBS (Map.keys env)
    --       yield $ M.Have (ecodeOnePeiece 55)
           let msg = trace "UYYYYYY" M.UnChoke
           yield msg
           networkSource


sendNetworkResp :: (MonadReader (Map.Map Int B.ByteString) m
                   ,MonadState ActionST m)
                =>  Source m M.Message
sendNetworkResp = do
    st <- get
    case sendReqLog st of
        Nothing -> return ()
        Just r@(i, off, size) -> do
            env <- ask
            let bs = getChunk env r
            let k = trace ("!!!!!!!!!!!!!!!! "++(show i ++" "++show off)) i
            yield $ M.Piece (k, off, bs)
            sendNetworkResp



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


getChunk ::  Map.Map Int B.ByteString -> (Int, Int, Int) -> B.ByteString
getChunk m (index, offset, size) =
    let mbs = Map.lookup index m
    in case mbs of
            Nothing -> error  "Impossible"
            Just bs -> B.take size (B.drop offset bs)
