{-# LANGUAGE FlexibleContexts,
             ScopedTypeVariables,
             MultiParamTypeClasses,
             FlexibleInstances,
             StandaloneDeriving,
             UndecidableInstances
  #-}

module PriorityQueueP (
 PQueueChan (),
 peekByPriority,
) where

import Data.List
import Control.Monad.MonadStateP
import EffectCapabilities
import QueueP
import {-# SOURCE #-} ExampleP

data PQueueChan = PQueueChan

queueState :: QState ReadPerm
queueState = fromChannel PQueueChan $ receive ReadPerm

peekByPriority :: (Ord s, MonadStateP QState [s] m) => (s -> s -> Ordering) -> m (Maybe s)
peekByPriority comp = do queue <- fromCapT queueState getp 
                         if null queue
                            then return Nothing
                            else return (Just $ maximumBy comp queue)

