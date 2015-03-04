{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.MonadErrorP (      
       MonadErrorP (..),
       ThrowPerm (..),
       CatchPerm (..),
       TCPerm (..),
       ImpliesEx(..),
) where

import Control.Monad.Error
import EffectCapabilities
import GHC.Exts (Constraint)

-- the lattice of Throw/Catch pemrmissions

data ThrowPerm = ThrowPerm
data CatchPerm = CatchPerm
data TCPerm    = TCPerm

type family SecretImpliesEx (a :: *) (b :: *) :: Constraint where
  SecretImpliesEx TCPerm ThrowPerm = ()
  SecretImpliesEx TCPerm CatchPerm = ()
  SecretImpliesEx a a = ()
  SecretImpliesEx a () = ()

class SecretImpliesEx a b => ImpliesEx a b
instance SecretImpliesEx a b => ImpliesEx a b

class (Monad m, Error e) => MonadErrorP c e m | c m -> e where
    throwErrorp :: (ImpliesEx ac ThrowPerm, Capability c ImpliesEx) => e -> CapT (c ac) m a
    catchErrorp :: (ImpliesEx ac CatchPerm, Capability c ImpliesEx) => m a -> (e -> m a) -> CapT (c ac) m a
