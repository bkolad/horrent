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
import Control.Monad.Trans.Class
import Control.Monad.Writer.Class

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
    T.putStrLn "BL!!!!!!"
    T.putStrLn l



data LoggerT h m a = L { runLogger:: h -> m a }


instance (Monad m) => Functor (LoggerT h m) where
    fmap f (L fun) = L $
      \h -> do x <- fun h
               return $ f x


instance (Monad m) => Applicative (LoggerT h m) where
    pure x = L $ \h -> return x
    (L fo) <*> (L x) = L $ \h -> (fo h) <*> (x h)


instance (Monad m) => Monad (LoggerT h m) where
    return = pure
    (L x) >>= fx = L $ \h ->
        do k <- (x h)
           runLogger (fx k) h




class Logable a where
   logM :: a -> T.Text -> IO ()


class (Monad m) => MonadLogger m where
    logMessage :: T.Text -> m ()


instance (Logable l) => MonadTrans (LoggerT l) where
    lift m = L $ \h -> m


instance (Logable l, MonadIO m) => MonadIO (LoggerT l m) where
    liftIO = lift . liftIO


instance (Logable l, MonadIO m) => MonadLogger (LoggerT l m) where
    logMessage x = L $ \h -> liftIO $ logM h x


instance MonadLogger IO where
    logMessage x = T.putStrLn x


-- =================

instance Logable BasicLogger where
    logM = logMsg



-- ===============


xx :: (MonadIO m, MonadLogger m) => m Int
xx = do
    liftIO $ print "lalal"
    logMessage "oooouuuu"
    return 2



yy :: IO Int
yy = do
    bl <- start
    runLogger xx bl
