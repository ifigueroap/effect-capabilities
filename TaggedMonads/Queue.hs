{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverlappingInstances, IncoherentInstances, UndecidableInstances #-}

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
import Control.Monad.MonadStatePV
import Control.Monad.MonadErrorPV
import EffectCapabilities

data QState p = QState p

instance Capability QState ImpliesRW where
  attenuate (QState _) perm = QState perm 

enqueue :: forall n m s.(MonadStatePV QState n m [s], TWith (QState ()) n m) => s -> m ()
enqueue x = let ?n = undefined :: n () in do
  queue  <- getpv `withCapability` QState ReadPerm
  putpv (queue ++ [x]) `withCapability` QState WritePerm
                  
dequeue :: forall n m s.(MonadStatePV QState n m [s], TWith (QState ()) n m) => m s
dequeue = let ?n = undefined :: n () in do
  queue <- getpv `withCapability` QState ReadPerm
  putpv (tail queue) `withCapability` QState WritePerm
  return $ head queue

data QError p = QError p

instance Capability QError ImpliesEx where
 attenuate (QError _) perm = QError perm

-- instance Send ExampleChan QError TCPerm where
--  receive perm = return $ QError perm

dequeueEx :: (?qs :: qs (), ?qe :: qe (),
  MonadStatePV QState qs m [s],    TWith (QState ()) qs  m,
  MonadErrorPV QError qe m String, TWith (QError ()) qe m) => m s
dequeueEx = do
  queue <- let ?n = ?qs in getpv `withCapability` (QState ReadPerm)
  if null queue
    then let ?n = ?qe in throwErrorpv "Queue is empty" `withCapability` (QError ThrowPerm)
    else do let ?n = ?qs in putpv (tail queue) `withCapability` (QState WritePerm)
            return $ head queue  

