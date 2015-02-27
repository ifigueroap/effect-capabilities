{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances, OverlappingInstances, IncoherentInstances #-}
{-# LANGUAGE ImplicitParams #-}

module Control.Monad.MonadStatePV (
       MonadStatePV (..),
       ReadPerm (..),
       WritePerm (..),
       RWPerm (..),
       ImpliesRW(..)
) where

import Control.Monad.Mask
import Control.Monad.MonadStateP
import Control.Monad.Reader
import Control.Monad.Views
import EffectCapabilities

class (Monad m, Monad n, MonadStateP c s n, TWith (c ()) n m) => MonadStatePV c n m s where

  getpv :: (?n :: n (), ImpliesRW perm ReadPerm, Capability c ImpliesRW) => CapT (c perm) m s
  getpv = do
    c <- ask 
    mapCapT (from $ tag c) getp
    where tag c = structure (attenuate c ()) :: n :><: m
   
  putpv :: (?n :: n (), ImpliesRW perm WritePerm, Capability c ImpliesRW) => s -> CapT (c perm) m ()
  putpv s = do
    c <- ask 
    mapCapT (from $ tag c) $ putp s
    where tag c = structure (attenuate c ()) :: n :><: m

instance (Monad m, Monad n, MonadStateP c s n, TWith (c ()) n m) => MonadStatePV c n m s


