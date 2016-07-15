{-# LANGUAGE DeriveFunctor #-}

module Interpreters.Action ( ActionF (..)
              , setStatusF
              , sendInterestedF
              , logF
              , requestNextAndUpdateGlobalF
              , sendRequestF
              , getSizeInfoF
              , readDataWithTimeoutF
              , saveToFileF
              , getPendingPieceF
              , throwF
              , Action
              , Free(Free,Pure)
              ) where

import Control.Monad.Free (Free(Free,Pure))
import qualified Types as TP
import qualified Data.ByteString as B



data ActionF a = SendInterested a
                | Log String a
                | ReqNextAndUpdate [Int] (Maybe Int -> a)
                | SendRequest (Int, Int, Int) a
                | SetStatus Int TP.PiceInfo a
                | ReqSizeInfo (TP.SizeInfo -> a)
                | ReadData Int (B.ByteString -> a)
                | SaveToFile String B.ByteString a
                | GetPendingPiece (Maybe Int -> a)
                | UnChoke a
                | Throw TP.ExeptionType a
                deriving (Functor)


type Action = Free ActionF

sendInterestedF :: Action ()
sendInterestedF = Free $ SendInterested (Pure ())


logF :: String -> Action ()
logF str = Free $ Log str (Pure ())


requestNextAndUpdateGlobalF :: [Int] -> Action (Maybe Int)
requestNextAndUpdateGlobalF pieces =
    Free $ ReqNextAndUpdate pieces Pure


sendRequestF :: (Int, Int, Int) -> Action ()
sendRequestF req = Free $ SendRequest req (Pure ())


setStatusF :: Int -> TP.PiceInfo -> Action ()
setStatusF x status = Free $ SetStatus x status (Pure ())


getSizeInfoF :: Action TP.SizeInfo
getSizeInfoF = Free $ ReqSizeInfo Pure


readDataWithTimeoutF :: Int -> Action B.ByteString
readDataWithTimeoutF t = Free $ ReadData t Pure


saveToFileF :: String -> B.ByteString -> Action ()
saveToFileF fN c = Free $ SaveToFile fN c (Pure ())


getPendingPieceF :: Action (Maybe Int)
getPendingPieceF = Free $ GetPendingPiece Pure


unChokeF :: Action ()
unChokeF = Free $ UnChoke (Pure ())


throwF ::TP.ExeptionType ->  Action ()
throwF x = Free $ Throw x (Pure ())
