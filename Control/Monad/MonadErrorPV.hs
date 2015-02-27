{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE UndecidableInstances, OverlappingInstances, IncoherentInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Monad.MonadErrorPV (
       MonadErrorPV (..),
       ThrowPerm (..),
       CatchPerm (..),
       TCPerm (..),
       ImpliesEx
) where

import Control.Monad.Error
import Control.Monad.ErrorTP
import Control.Monad.MonadErrorP
import Control.Monad.Mask
import Control.Monad.Views
import EffectCapabilities
import Control.Monad.Reader

class (Error e, Monad m, Monad n, MonadErrorP c e n) => MonadErrorPV c n m e where

  throwErrorpv :: (TWith (c ()) n m, ?n :: n (), Capability c ImpliesEx, ImpliesEx perm ThrowPerm)
                  => e -> CapT (c perm) m a
  throwErrorpv e = do
      c <- ask 
      mapCapT (from $ tag c) $ throwErrorp e
    where tag c = structure (attenuate c ()) :: n :><: m

  catchErrorpv :: (TWith (c ()) n m, ?n :: n (), Capability c ImpliesEx, ImpliesEx perm CatchPerm)
                  => m a -> (e -> m a) -> CapT (c perm) m a
  catchErrorpv ma hnd = do
      c <- ask 
      mapCapT (from $ tag c) $ catchErrorp (to (tag c) ma) (\ e -> (to (tag c) (hnd e)))
    where tag c = structure (attenuate c ()) :: n :><: m 

instance (Error e, Monad n, Monad m, MonadErrorP c e n) => MonadErrorPV c n m e
