{-# LANGUAGE OverloadedStrings
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , FlexibleContexts #-}

module Logger.BasicLogger where

import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import Control.Concurrent.STM.TChan (TChan, newTChan, writeTChan, readTChan)
import Control.Concurrent (forkIO)
import Control.Monad.STM (atomically)
import Control.Monad (forever)
import Control.Monad.IO.Class

data BasicLogger =
    BasicLogger { unlogger :: TChan T.Text }

type Logger a = (a -> IO())


start :: IO BasicLogger
start = do
  c <- atomically newTChan
  forkIO (process c)
  return $ BasicLogger c

logMsg :: BasicLogger -> T.Text -> IO ()
logMsg (BasicLogger l) msg =
    atomically $ writeTChan l msg

logString l msg = logMsg l (T.pack msg)

process :: TChan T.Text -> IO ()
process chan = forever $ do
    l <- atomically $ readTChan chan
    T.putStrLn l



data Handle = H
            | G
            deriving Show


data LoggerT m a = L { runLogger:: Handle -> m a }

instance (Monad m) => Functor (LoggerT m) where
    fmap f (L fun) = L $
      \h -> do x <- fun h
               return $ f x

instance (Monad m) => Applicative (LoggerT m) where
    pure x = L $ \h -> return x
    (L fo) <*> (L x) = L $ \h -> (fo h) <*> (x h)


instance (Monad m) => Monad (LoggerT m) where
    return = pure
    (L x) >>= fx = L $ \h ->
        do k <- (x h)
           runLogger (fx k) h


printT h = print ("blabal " ++ show h)
printT2 h = print ("blabal222 " ++ show h)

test :: LoggerT IO ()
test =  do L printT
           L printT2

tt = runLogger test H



class (Monad m) => MonadLogger m where
    logMessage :: T.Text -> m ()

instance MonadLogger IO where
    logMessage x = T.putStrLn x


instance (MonadIO m) => MonadLogger (LoggerT m) where
    logMessage x = L $ \h -> liftIO $ printT h

instance (MonadIO m) => MonadIO (LoggerT m) where
    liftIO = undefined--lift . liftIO

xx :: (MonadIO m, MonadLogger m) => m Int
xx = do
    --liftIO $ print "lalal"
    logMessage "oooo"
    return 2


yy :: IO Int
yy = runLogger xx H


{--

instance (Monad m) => MonadLogger l (LoggerT l m) where
    logMessage x = undefined --}
