{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

-- There is a bug when using Deriving Generic and hs-boot files in GHC <= 7.6.3
-- https://ghc.haskell.org/trac/ghc/ticket/7878

-- Implementation of the Queue module, considering all enhancements
-- from Section 4.

module TaggedMonads.Queue (
       enqueue,
       dequeue,
       dequeueEx,
       QState (),
       QError (),
) where

import Control.Monad.Trans
import Control.Monad.Mask
import Control.Monad.Views
import Control.Monad.MonadStatePV
import Control.Monad.MonadErrorPV
import EffectCapabilities
import {-# SOURCE #-} ExamplesTaggedMonads

data QState p = QState p

instance Capability QState ImpliesRW where
  attenuate (QState _) perm = QState perm 

enqueue :: (MonadStatePV QState [s] n m) => n :><: m -> s -> m ()
enqueue tag x = do
  queue  <- getpv tag `withCapability` QState ReadPerm
  putpv tag (queue ++ [x]) `withCapability` QState WritePerm
                  
dequeue :: (MonadStatePV QState [s] n m) => n :><: m -> m s
dequeue tag = do
  queue <- getpv tag `withCapability` QState ReadPerm
  putpv tag (tail queue) `withCapability` QState WritePerm
  return $ head queue

data QError p = QError p

instance Capability QError ImpliesEx where
  attenuate (QError _) perm = QError perm

instance Send ExampleChan QError TCPerm where
 receive perm = return $ QError perm

dequeueEx :: (MonadStatePV QState [s] n m, MonadErrorPV QError String n' m)
             => n :><: m -> n' :><: m -> m s
dequeueEx vqs vqe = do
  queue <- getpv vqs `withCapability` (QState ReadPerm)
  if null queue
    then throwErrorpv vqe "Queue is empty" `withCapability` (QError ThrowPerm)
    else do putpv vqs (tail queue) `withCapability` (QState WritePerm)
            return $ head queue  

