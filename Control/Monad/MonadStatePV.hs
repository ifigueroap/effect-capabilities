{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Monad.MonadStatePV (
       MonadStatePV (..),
       ReadPerm (..),
       WritePerm (..),
       RWPerm (..),
       ImpliesRW(..),
) where

import Control.Monad.Trans
import Control.Monad.Zipper
import Control.Monad.Mask
import Control.Monad.MonadStateP
import Control.Monad.Reader
import Control.Monad.Views
import EffectCapabilities

class (Capability c ImpliesRW, Monad m, Monad n, MonadStateP c s n, TWith (c ()) n m) => MonadStatePV c s n m where

  getpv :: (ImpliesRW perm ReadPerm) => n :><: m -> CapT (c perm) m s
  getpv tag = do
    c <- ask 
    mapCapT (from tag) getp    
   
  putpv :: (ImpliesRW perm WritePerm) => n :><: m -> s -> CapT (c perm) m ()
  putpv tag s = do
    c <- ask 
    mapCapT (from tag) $ putp s

instance (Capability c ImpliesRW, Monad m, Monad n, MonadStateP c s n, TWith (c ()) n m) => MonadStatePV c s n m
