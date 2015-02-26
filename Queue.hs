{-# LANGUAGE TypeOperators,
             FlexibleContexts,
             ScopedTypeVariables,
             DefaultSignatures,
             MultiParamTypeClasses,
             DeriveGeneric,
             FlexibleInstances
  #-}

-- There is a bug when using Deriving Generic and hs-boot files in GHC <= 7.6.3
-- https://ghc.haskell.org/trac/ghc/ticket/7878

-- Implementation of the Queue module, considering all enhancements
-- from Section 4.

module Queue (
       enqueue,
       dequeue,
       dequeueEx,
       dequeueErr,
       QState (),
       QError ()
) where

import Control.Monad.MonadErrorP
import Control.Monad.MonadStateP
import EffectCapabilities
import GHC.Generics
import {-# SOURCE #-} PriorityQueue
import {-# SOURCE #-} ExamplesSection4

-- Code from Section 4.1: Private State

data QState p = QState p -- deriving Generic

instance Capability QState ImpliesRW where
 attenuate (QState _) perm = QState perm

enqueue :: MonadStateP QState [s] m => s -> m ()
enqueue x = do queue  <- getp `withCapability` (QState ReadPerm)
               putp (queue ++ [x]) `withCapability` (QState WritePerm)               

dequeue :: MonadStateP QState [s] m => m s
dequeue = do queue <- getp `withCapability` (QState ReadPerm)
             putp (tail queue) `withCapability` (QState WritePerm)
             return $ head queue        

-- Section 4.2: Sharing state with PriorityQueue

instance Send PQueueChan QState ReadPerm where
 receive perm = return $ QState perm

-- Section 4.3: Protected exceptions 

data QError p = QError p

instance Capability QError ImpliesEx where
 attenuate (QError _) perm = QError perm

instance Send ExampleChan QError TCPerm where
 receive perm = return $ QError perm

dequeueEx :: (MonadStateP QState [s] m, MonadErrorP QError String m) => m s
dequeueEx = do queue <- getp `withCapability` (QState ReadPerm)
               if null queue
                  then throwErrorp "Queue is empty" `withCapability` (QError ThrowPerm)
                  else do putp (tail queue) `withCapability` (QState WritePerm)
                          return $ head queue

dequeueErr :: (MonadStateP QState [s] m, MonadErrorP QError String m) => m s
dequeueErr = fromCapT (QError CatchPerm) $ catchErrorp dequeueEx error
                    

