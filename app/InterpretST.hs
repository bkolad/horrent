module InterpretST where

import Control.Monad.State.Lazy
import Data.Conduit
import qualified Message as M
import qualified Data.ByteString.Char8 as BC
import qualified TubeDSL as TDSL


import Action


data ActionST = GotInterested Bool
              | GotLog [String]
              | GotReqNext
              | GotSendReq
              | GotSetStatus
              deriving Show

-- TODO write conduit pipe with ActionST state


interpret :: Action a -> State ActionST a
interpret program =
    case program of
        Free (SendInterested c) -> undefined

        Free (Log str c) -> do
            st <- get
            case st of
                GotLog x -> do
                    put (GotLog $ str:x)
                    interpret c


        Free (ReqNextAndUpdate pieces fun) -> undefined

        Free (SendRequest req c) -> undefined

        Free (SetStatus x status c) -> undefined

        Pure x -> return x


networkSource :: Source (State ActionST) M.Message
networkSource = yield M.KeepAlive

fileSink ::  Sink (String, BC.ByteString) (State ActionST) ()
fileSink = do
    x <- await
    case x of
        Nothing -> return ()
        Just s -> fileSink


testTube :: State ActionST ()
testTube = networkSource
     $= (transPipe interpret (TDSL.recMessage undefined))
     $$ fileSink


test = runState testTube (GotLog [])
