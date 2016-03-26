module InterpretST where

import Control.Monad.State.Lazy
import Data.Conduit
import qualified Message as M
import qualified Data.ByteString.Char8 as BC
import qualified TubeDSL as TDSL
import qualified Types as TP
import qualified Peer as P


import Action


data ActionST = ActionST { logS :: [String]
                         , reqNext :: [Int]
                         , sendInterested :: Bool
                         , sendReq :: Bool
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

        Free (ReqNextAndUpdate pieces fun) -> undefined

        Free (SendRequest req c) -> undefined

        Free (SetStatus x status c) -> undefined

        Pure x -> return x


pieces = [1,7,88,99]
encodedPieces = P.pieceLsToBS pieces

startState = ActionST { logS = []
                      , reqNext = []
                      , sendInterested = False
                      , sendReq = False
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
