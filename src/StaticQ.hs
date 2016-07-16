module StaticQ
    ( makeQueueFromList
    , spawnNThreadsAndWait
    , loop
    ) where

import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Control.Monad.STM as STM
import Control.Monad (replicateM)
import Control.Concurrent.Async


data SQueue a = SQueue (TQ.TQueue a)


makeQueueFromList :: (Foldable t, Traversable t)
                  => t a
                  -> IO (SQueue a)
makeQueueFromList ls = STM.atomically $
     do queue <- TQ.newTQueue
        mapM_ (TQ.writeTQueue queue) ls
        return $ SQueue queue



readFromQueue :: Show a => SQueue a -> IO (Maybe a)
readFromQueue (SQueue queue) = STM.atomically $ TQ.tryReadTQueue queue


spawnNThreadsAndWait :: Int -> IO b -> IO [b]
spawnNThreadsAndWait n action = do
    x <- replicateM n (async action)
    mapM wait x



loop :: Show a => SQueue a -> (a -> IO b)-> [b]-> IO [b]
loop queue action acc = do
    x <- readFromQueue queue
    case x of
        Just k -> do
            x <- action  k
            loop queue action (x:acc)
        Nothing ->
            return acc



{--
doIO x = do tI <- myThreadId
            threadDelay 100000
            print $ (show x) ++"  "++ (show tI)


tryQueue = do q2 <- makeQueueFromList ([1, 2 .. 10] )
              print "Queue"
              spawnNThreadsAndWait 1 (loop q2 doIO)
--}
