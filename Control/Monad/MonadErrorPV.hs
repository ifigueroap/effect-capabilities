{-# LANGUAGE TypeOperators,
             MultiParamTypeClasses,
             FunctionalDependencies,
             ScopedTypeVariables,
             FlexibleInstances,
             UndecidableInstances,
             ConstraintKinds,
             FlexibleContexts,
             BangPatterns
  #-}

module MonadErrorPV (
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

class (MonadErrorP c e n, TWith (c ()) n m) 
      => MonadErrorPV c n m e | c -> n e where

      throwError :: (Capability c ImpliesEx, ImpliesEx perm ThrowPerm)
                    => e -> CapT (c perm) m a
      throwError e = do c <- ask 
                        mapCapT (from $ tag c) $ throwErrorp e
                     where tag c = structure (attenuate c ()) :: n :><: m
      catchError :: (Capability c ImpliesEx, ImpliesEx perm CatchPerm)
                    => m a -> (e -> m a) -> CapT (c perm) m a
      catchError ma hnd = do c <- ask 
                             mapCapT (from $ tag c) $ 
                                 catchErrorp (to (tag c) ma)
                                             (\ e -> (to (tag c) (hnd e)))
                          where tag c = structure (attenuate c ()) :: n :><: m
 
instance (MonadErrorP c e n, TWith (c ()) n m) => MonadErrorPV c n m e
