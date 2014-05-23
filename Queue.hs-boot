{-# LANGUAGE TypeOperators,
             FlexibleContexts,
             ScopedTypeVariables,
             MultiParamTypeClasses,
             DeriveGeneric,
             FlexibleInstances
  #-}

module Queue (QState ()) where

import EffectCapabilities
import GHC.Generics
import MonadStatePV
import {-# SOURCE #-} PriorityQueue
import Control.Monad.State.StateTP

data QState a = QState a
instance Generic (QState a)


instance Capability QState a
instance Send PQueueChan QState ReadPerm



