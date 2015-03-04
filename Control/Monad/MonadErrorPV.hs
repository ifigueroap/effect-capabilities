{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.MonadErrorPV (
       MonadErrorPV (..),
       ThrowPerm (..),
       CatchPerm (..),
       TCPerm (..),
       ImpliesEx(..),
) where

import Control.Monad.Error
import Control.Monad.ErrorTP
import Control.Monad.MonadErrorP
import Control.Monad.Mask
import Control.Monad.Views
import EffectCapabilities
import Control.Monad.Reader

class (Capability c ImpliesEx, Error e, Monad m, Monad n, MonadErrorP c e n, TWith (c ()) n m) => MonadErrorPV c e n m where

  throwErrorpv :: (ImpliesEx perm ThrowPerm) => n :><: m -> e -> CapT (c perm) m a
  throwErrorpv tag e = do
      c <- ask 
      mapCapT (from tag) $ throwErrorp e

  catchErrorpv :: (ImpliesEx perm CatchPerm) => n :><: m -> m a -> (e -> m a) -> CapT (c perm) m a
  catchErrorpv tag ma hnd = do
      c <- ask 
      mapCapT (from tag) $ catchErrorp (to tag ma) (\ e -> (to tag (hnd e)))


instance (Capability c ImpliesEx, Error e, Monad n, Monad m, MonadErrorP c e n, TWith (c ()) n m)
         => MonadErrorPV c e n m
