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
       module Control.Monad.ErrorTP,
       module Control.Monad.Error,
       module Control.Monad.Mask,
       module Control.Monad.Identity
) where

import Control.Monad.Error
import Control.Monad.ErrorTP
import Control.Monad.MonadErrorP
import Control.Monad.Mask
import Control.Monad.Views
import EffectCapabilities
import Control.Monad.Reader
import Control.Monad.Identity

class (Capability c ImpliesEx, Error e, Monad m, Monad n, MonadErrorP c e n, TWith (c ()) n m) => MonadErrorPV c e n m where

  throwErrorpv :: (ImpliesEx perm ThrowPerm, View v) => n `v` m -> e -> CapT (c perm) m a
  throwErrorpv tag e = mapCapT (from tag) $ throwErrorp e

  catchErrorpv :: (ImpliesEx perm CatchPerm) => n :><: m -> m a -> (e -> m a) -> CapT (c perm) m a
  catchErrorpv tag ma hnd = mapCapT (from tag) $ catchErrorp (to tag ma) (to tag . hnd)


instance (Capability c ImpliesEx, Error e, Monad n, Monad m, MonadErrorP c e n, TWith (c ()) n m)
         => MonadErrorPV c e n m
