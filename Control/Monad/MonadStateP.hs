{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleContexts,
             FlexibleInstances,
             UndecidableInstances,
             IncoherentInstances,
             TypeOperators
  #-}

module Control.Monad.MonadStateP (
  MonadStateP (..),
  ReadPerm (..),
  WritePerm (..),
  RWPerm (..),
  ImpliesRW(..)
) where

import EffectCapabilities
import Data.List

-- the lattice of Read/Write permissions

data WritePerm = WritePerm deriving Show
data ReadPerm  = ReadPerm deriving Show
data RWPerm    = RWPerm deriving Show

-- To construct a lattice of permissions

class SecretImpliesRW a b
class SecretImpliesRW a b => ImpliesRW a b
instance SecretImpliesRW a a 
instance SecretImpliesRW a () 
instance SecretImpliesRW a b => ImpliesRW a b 

instance SecretImpliesRW RWPerm ReadPerm
instance SecretImpliesRW RWPerm WritePerm

class Monad m => MonadStateP c s m | m -> s where
  getp :: (Capability c ImpliesRW, ImpliesRW p ReadPerm) => CapT (c p) m s
  putp :: (Capability c ImpliesRW, ImpliesRW p WritePerm) => s -> CapT (c p) m ()
