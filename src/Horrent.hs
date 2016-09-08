{-# LANGUAGE MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           #-}

module Horrent where

import Control.Monad.Except
import Control.Monad.IO.Class
import Logger.BasicLogger


class (MonadLogger m l, MonadIO m, MonadError String m) => MonadHorrent m l

instance (MonadLogger m l, MonadIO m, MonadError String m) => MonadHorrent m l
