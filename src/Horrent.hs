{-# LANGUAGE MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           #-}

module Horrent where

import Control.Monad.Except
import Control.Monad.IO.Class
import Logger.BasicLogger


class (MonadLogger m l, MonadIO m, MonadError e m) => MonadHorrent m l e

instance (MonadLogger m l, MonadIO m, MonadError e m) => MonadHorrent m l e
