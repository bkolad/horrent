module Utils (io2ExceptT)
    where

import Control.Exception (try, SomeException)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (ExceptT(ExceptT))


io2ExceptT :: (MonadIO m) => IO a -> ExceptT String m a
io2ExceptT = ExceptT
         . liftIO
         . liftM (fmapL show)
         . (try :: IO a -> IO (Either SomeException a))

fmapL :: (a -> b) -> Either a r -> Either b r
fmapL f (Left x) = Left $ f x
fmapL _ (Right x) = Right x
