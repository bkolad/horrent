module Logger.BasicLogger where

import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import Control.Concurrent.STM.TChan (TChan, newTChan, writeTChan, readTChan)
import Control.Concurrent (forkIO)
import Control.Monad.STM (atomically)
import Control.Monad (forever)

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
-- TODO replace by LoggerT
data LoggerIO a = L { run:: Handle -> IO a }

instance Functor LoggerIO where
    fmap f (L fun) = L $
      \h -> do x <- fun h
               return $ f x

instance Applicative LoggerIO where
    pure x = L $ \h -> return x
    (L fo) <*> (L x) = L $ \h -> (fo h) <*> (x h)


instance Monad LoggerIO where
    return = pure
    (L x) >>= fx = L $ \h ->
        do k <- (x h)
           run (fx k) h


printT h = print ("blabal " ++ show h)
printT2 h = print ("blabal222 " ++ show h)

test :: LoggerIO ()
test =  do L printT
           L printT2

tt = run test H
