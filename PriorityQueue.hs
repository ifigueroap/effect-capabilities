{-# LANGUAGE TypeOperators,
             FlexibleContexts,
             ScopedTypeVariables,
             MultiParamTypeClasses,
             FlexibleInstances,
             StandaloneDeriving,
             UndecidableInstances
  #-}

module PriorityQueue (
 PQueueChan (),
 peekByPriority,
) where

import Data.List
import Control.Monad.Error
import MonadStatePV
import EffectCapabilities
import Queue
import {-# SOURCE #-} Example

data PQueueChan = PQueueChan

-- instance Send ExampleChan QState ReadPerm
--  where receive = receive

queueState :: QState ReadPerm
queueState = fromChannel PQueueChan $ receive ReadPerm

peekByPriority :: (Ord s, MonadStatePV QState n m [s]) => (s -> s -> Ordering) -> m (Maybe s)
peekByPriority comp = do
               queue <- fromCapT queueState get 
               if null queue
                  then return Nothing
                  else return (Just $ maximumBy comp queue)

