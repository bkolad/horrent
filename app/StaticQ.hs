module StaticQ (makeQueueFromList, spawnNThreadsAndWait, loop) where

import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Concurrent
import qualified Control.Monad.STM as STM
import Control.Monad
import Control.Concurrent.Async

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



spawnNThreadsAndWait :: Int -> IO () -> IO ()    
spawnNThreadsAndWait n action = 
     do x <- replicateM n (async action)
        mapM wait x
        return () 
        
        

loop :: Show a => SQueue a -> (a -> IO ())-> IO ()
loop queue action = 
     do x <- readFromQueue queue
        case x of 
             Just k -> (action  k) >>
                       loop queue action
             Nothing -> return()

             
             
             
doIO x = do tI <- myThreadId
            threadDelay 1000000
            print $ (show x) ++"  "++ (show tI)
             
             
tryQueue = do q2 <- makeQueueFromList ([1, 2 .. 10] )
              print "Queue"
              spawnNThreadsAndWait 2 (loop q2 doIO)
              
              
              