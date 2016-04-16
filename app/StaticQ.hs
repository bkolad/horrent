module StaticQ (makeQueueFromList, spawnNThreadsAndWait, loop) where

import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Concurrent
import qualified Control.Monad.STM as STM
import Control.Monad
import Control.Concurrent.Async
import Control.Exception
import System.Timeout


-- https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
-- ma

{--
SQueue wrapper on TQueue. It allows creation TQueue from static list
and prohibits wiriting to queue later on.

This limitation make queue less flexoble
but simplifies control flow at the same time.

--}

data SQueue a = SQueue (TQ.TQueue a)


makeQueueFromList :: Foldable t => t a -> IO (SQueue a)
makeQueueFromList ls = STM.atomically $
     do queue <- TQ.newTQueue
        foldM (\acc x -> TQ.writeTQueue queue x) () ls
        return $ SQueue queue



readFromQueue :: Show a => SQueue a -> IO (Maybe a)
readFromQueue (SQueue queue) = STM.atomically $ TQ.tryReadTQueue queue



spawnNThreadsAndWait :: Int -> IO b -> IO [b]
spawnNThreadsAndWait n action =
     do x <- replicateM n (async action)
        mapM wait x
        --return ()


tOut = 45*60 * 1000000

loop :: Show a => SQueue a -> (a -> IO b)-> [b]-> IO [b]
loop queue action acc =
     do x <- readFromQueue queue
        case x of
             Just k -> do
                 x <- (action  k)
                 loop queue action (x:acc)

             Nothing -> return acc



{--
doIO x = do tI <- myThreadId
            threadDelay 100000
            print $ (show x) ++"  "++ (show tI)


tryQueue = do q2 <- makeQueueFromList ([1, 2 .. 10] )
              print "Queue"
              spawnNThreadsAndWait 1 (loop q2 doIO)
--}
