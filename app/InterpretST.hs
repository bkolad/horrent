module InterpretST where

import Control.Monad.State.Lazy

import Action


data ActionST = GotInterested Bool
              | GotLog [String]
              | GotReqNext
              | GotSendReq
              | GotSetStatus

-- TODO write conduit pipe with ActionST state


interpret :: Action a -> State ActionST a
interpret program =
    case program of
        Free (SendInterested c) -> undefined

        Free (Log str c) -> undefined

        Free (ReqNextAndUpdate pieces fun) -> undefined

        Free (SendRequest req c) -> undefined

        Free (SetStatus x status c) -> undefined

        Pure x -> return x
