{-# LANGUAGE FlexibleContexts,
             FlexibleInstances,
             MultiParamTypeClasses,
             TypeOperators,
             ScopedTypeVariables,
             FunctionalDependencies,
             UndecidableInstances
  #-}


module MonadStatePV (
       MonadStatePV (..),
       ReadPerm (..),
       WritePerm (..),
       RWPerm (..),
       ImpliesRW(..)
) where

import Control.Monad.State
import Control.Monad.Mask
import Control.Monad.Views
import Control.Monad.MonadStateP
import EffectCapabilities
import Control.Monad.State.StateTP
import Data.List
import Control.Monad.Reader

class (Monad m, MonadStateP c s n, TWith (c ()) n m)
      => MonadStatePV c n m s | c -> n s where
      get :: (ImpliesRW perm ReadPerm, Capability c ImpliesRW) => CapT (c perm) m s
      get = do c <- ask 
               mapCapT (from $ tag c) getp
            where tag c = structure (attenuate c ()) :: n :><: m
   
      put :: (ImpliesRW perm WritePerm, Capability c ImpliesRW) => s -> CapT (c perm) m ()
      put s = do c <- ask 
                 mapCapT (from $ tag c) $ putp s
              where tag c = structure (attenuate c ()) :: n :><: m

instance (Monad m, MonadStateP capability s n,TWith (capability ()) n m)
         => MonadStatePV capability n m s
