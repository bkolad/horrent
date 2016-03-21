{-# LANGUAGE DeriveFunctor #-}

module HorrentF (Action (SendInterested
                          , Log
                          , ReqNextAndUpdate
                          , SendRequest
                          , SetStatus)
                          , setStatusF
                          , sendInterestedF
                          , logF
                          , requestNextAndUpdateGlobalF
                          , sendRequestF
                          , ActionF
                          , Free(Free,Pure)) where

import Control.Monad.Free (Free(Free,Pure))
import qualified Types as TP

data Action a = SendInterested a
                | Log String a
                | ReqNextAndUpdate [Int] TP.GlobalPiceInfo ((Maybe Int) -> a)
                | SendRequest (Int, Int, Int) a
                | SetStatus Int TP.GlobalPiceInfo TP.PiceInfo a
                deriving (Functor)


type ActionF = Free Action

sendInterestedF :: ActionF ()
sendInterestedF = Free $ SendInterested (Pure ())


logF :: String -> ActionF ()
logF str = Free $ Log str (Pure ())


requestNextAndUpdateGlobalF :: [Int]
                            -> TP.GlobalPiceInfo
                            -> ActionF (Maybe Int)
requestNextAndUpdateGlobalF pieces global =
    Free $ ReqNextAndUpdate pieces global Pure


sendRequestF :: (Int, Int, Int) -> ActionF ()
sendRequestF req = Free $ SendRequest req (Pure ())


setStatusF :: Int -> TP.GlobalPiceInfo -> TP.PiceInfo -> ActionF ()
setStatusF x globab status = Free $ SetStatus x globab status (Pure ())
