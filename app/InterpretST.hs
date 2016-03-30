{-# LANGUAGE FlexibleContexts #-}

module InterpretST  where

import Control.Monad.State.Strict
import Control.Monad.Reader.Class
import Control.Monad.Reader

import qualified Message as M
import qualified Data.ByteString as B
import qualified Types as TP
import qualified Data.Map as Map
import qualified Data.List as L
import Action


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
          => TP.SizeInfo -> Action a -> m a
interpret sizeInfo program =
    case program of
        Free (SendInterested c) -> do
            modify (\st -> st {sendInterested = True})
            interpret sizeInfo c


        Free (Log str c) -> do
            modify (\st -> st {logS = str : logS st})
            interpret sizeInfo c


        Free (ReqNextAndUpdate pieces fun) -> do
                st <- get
                let reqLog = reqNextLog st
                    diff = pieces L.\\ reqLog
                case diff of
                    [] -> interpret sizeInfo (fun Nothing)
                    (x:xs) -> do
                        modify (\st -> st {reqNextLog = x : reqNextLog st})
                        interpret sizeInfo $ fun (Just x)


        Free (SendRequest r@(next, offset, chunkSize) c) -> do
            modify (\st -> st {sendReqLog = Just r})
            interpret sizeInfo c

        Free (SetStatus x status c) -> interpret sizeInfo c

        Free (ReqSizeInfo fun) ->
            interpret sizeInfo (fun sizeInfo)


        Pure x -> return x
