{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleContexts,
             FlexibleInstances,
             UndecidableInstances,
             IncoherentInstances
  #-}

module Control.Monad.MonadErrorP (      
       MonadErrorP (..),
       ThrowPerm (..),
       CatchPerm (..),
       TCPerm (..),
       ImpliesEx(..)
) where

import Control.Monad.Error
import EffectCapabilities

-- the lattice of Throw/Catch pemrmissions

data ThrowPerm = ThrowPerm
data CatchPerm = CatchPerm
data TCPerm    = TCPerm 

class SecretImpliesEx a b
class SecretImpliesEx a b => ImpliesEx a b
instance SecretImpliesEx a a 
instance SecretImpliesEx a ()
instance SecretImpliesEx a b => ImpliesEx a b 

instance SecretImpliesEx TCPerm ThrowPerm
instance SecretImpliesEx TCPerm CatchPerm

class (Monad m, Error e) => MonadErrorP c e m | c m -> e where
    throwErrorp :: (ImpliesEx ac ThrowPerm, Capability c ImpliesEx) => e -> CapT (c ac) m a
    catchErrorp :: (ImpliesEx ac CatchPerm, Capability c ImpliesEx) => m a -> (e -> m a) -> CapT (c ac) m a
