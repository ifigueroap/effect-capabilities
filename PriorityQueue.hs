{-# LANGUAGE FlexibleContexts,
             ScopedTypeVariables,
             MultiParamTypeClasses,
             FlexibleInstances,
             StandaloneDeriving,
             UndecidableInstances
  #-}

module PriorityQueue (
 PQueueChan (),
 peekBy,
) where

import Data.List
import Control.Monad.MonadStateP
import EffectCapabilities
import Queue
import {-# SOURCE #-} ExamplesSection4

data PQueueChan = PQueueChan

queueState :: QState ReadPerm
queueState = fromChannel PQueueChan $ receive ReadPerm

peekBy :: (Ord s, MonadStateP QState [s] m) => (s -> s -> Ordering) -> m (Maybe s)
peekBy comp = do queue <- fromCapT queueState getp 
                 if null queue
                   then return Nothing
                   else return (Just $ maximumBy comp queue)

peek :: (MonadStateP QState [s] m) => m (Maybe s)
peek = do queue <- fromCapT queueState getp 
          if null queue
            then return Nothing
            else return (Just $ head queue)                        

