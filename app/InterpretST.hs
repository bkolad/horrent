module InterpretST where

import Control.Monad.State.Lazy
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

import Action


data ActionST = ActionST { logS :: [String]
                         , reqNextLog :: [Int]
                         , sendInterested :: Bool
                         , sendReqLog :: [(Int,Int,Int)]
                         , setStatus :: [(Int, TP.PiceInfo)]
                         } deriving Show


interpret :: Action a -> State ActionST a
interpret program =
    case program of
        Free (SendInterested c) -> do
            modify (\st -> st {sendInterested = True})
            interpret c


        Free (Log str c) -> do
            modify (\st -> st {logS = str : logS st})
            interpret c

        Free (ReqNextAndUpdate pieces fun) -> do
            case pieces of
                [] -> interpret (fun Nothing)
                (x : xs) -> do
                    modify (\st -> st {reqNextLog = x : reqNextLog st})
                    interpret $ fun (Just x)

        Free (SendRequest r@(next, offset, chunkSize) c) -> do
            modify (\st -> st {sendReqLog = r : sendReqLog st})
            interpret c

        Free (SetStatus x status c) -> interpret c

        Pure x -> return x


pieces = [1,7,88,99]
encodedPieces = P.pieceLsToBS pieces

ecodeOnePeiece = P.fromIntToBs

startState = ActionST { logS = []
                      , reqNextLog = []
                      , sendInterested = False
                      , sendReqLog = []
                      , setStatus = []
                      }

initPeer = P.Peer { P.hostName  = "Some NAme"
                  , P.port = 22
                  , P.pieces = []
                  , P.infoHash = error "no info hash"
                  , P.globalStatus  = error "no global status"
                  , P.unChoked = False
                  , P.buffer = error "no info buffer"
                  , P.pieceHashes  = error "no p hashes"
                  , P.sizeInfo = error "no size info "
                  }



networkSource :: Source (State ActionST) M.Message
networkSource = do
    yield M.KeepAlive
    yield M.KeepAlive
    yield $ M.Bitfield encodedPieces
    yield $ M.Have (ecodeOnePeiece 55)
    yield $ M.Have (ecodeOnePeiece 21)
    yield M.UnChoke


fileSink ::  Sink (String, BC.ByteString) (State ActionST) ()
fileSink = do
    x <- await
    case x of
        Nothing -> return ()
        Just s -> fileSink


testTube :: State ActionST ()
testTube = networkSource
     $= (transPipe interpret (TDSL.recMessage initPeer))
     $$ fileSink

test = runState testTube startState



source :: (Monad m) => Source m Int
source = CL.sourceList [1..4]

toByteString :: (Monad m) => Conduit Int m BC.ByteString
toByteString = do
    x <- await
    case x of
        Nothing -> return ()
        Just r ->  do
            yield $ B.pack [fromIntegral r]
            toByteString

-- $= CL.map (\ x-> B.pack [fromIntegral x])

--pipe = CL.sourceList [1..10]  $$ CL.consume

--pipe =  (source $= toByteString $$ CB.sinkFile "output.txt")


mm = BC.pack (concat $ map show [0.. 100000])



pSize = 49152
splitBs :: Int -> Map.Map Int Int -> B.ByteString -> Map.Map Int Int
splitBs acc m bs =
    let (s, cont) = B.splitAt pSize bs
        newMap = Map.insert acc (B.length s) m
    in  if B.null cont then
            newMap
        else
            splitBs (acc+1) newMap cont


getChunk ::  Map.Map Int B.ByteString -> Int -> Int -> B.ByteString
getChunk m index offset size =
    let bs = Map.lookup index m
        sbs = B.take size (B.drop offset bs)
    in sbs




triple :: Monad m => Conduit a m a
-- show Triple conduit
triple = do
    ma <- await
    case ma of
        Nothing -> return ()
        Just a -> do
            CL.sourceList [a, a, a]
            triple
-- /show

kk = CL.sourceList [1..4] $=  CL.map (\ x-> B.pack [fromIntegral x]) $$ CL.mapM_ print
