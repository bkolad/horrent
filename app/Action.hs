{-# LANGUAGE DeriveFunctor #-}

module Action (ActionF (SendInterested
                          , Log
                          , ReqNextAndUpdate
                          , SendRequest
                          , SetStatus
                          , ReqSizeInfo)
                          , setStatusF
                          , sendInterestedF
                          , logF
                          , requestNextAndUpdateGlobalF
                          , sendRequestF
                          , getSizeInfoF
                          , Action
                          , Free(Free,Pure)) where

import Control.Monad.Free (Free(Free,Pure))
import qualified Types as TP

data ActionF a = SendInterested a
                | Log String a
                | ReqNextAndUpdate [Int] ((Maybe Int) -> a)
                | SendRequest (Int, Int, Int) a
                | SetStatus Int TP.PiceInfo a
                | ReqSizeInfo (TP.SizeInfo -> a)
                deriving (Functor)


type Action = Free ActionF

sendInterestedF :: Action ()
sendInterestedF = Free $ SendInterested (Pure ())


logF :: String -> Action ()
logF str = Free $ Log str (Pure ())


requestNextAndUpdateGlobalF :: [Int]
                            -> Action (Maybe Int)
requestNextAndUpdateGlobalF pieces =
    Free $ ReqNextAndUpdate pieces Pure


sendRequestF :: (Int, Int, Int) -> Action ()
sendRequestF req = Free $ SendRequest req (Pure ())


setStatusF :: Int -> TP.PiceInfo -> Action ()
setStatusF x status = Free $ SetStatus x status (Pure ())

getSizeInfoF :: Action TP.SizeInfo
getSizeInfoF = Free $ ReqSizeInfo Pure
