{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleContexts,
             FlexibleInstances,
             UndecidableInstances,
             IncoherentInstances,
             TypeOperators
  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Monad.MonadStateP (
  MonadStateP (..),
  ReadPerm (..),
  WritePerm (..),
  RWPerm (..),
  ImpliesRW(..),
) where

import EffectCapabilities
import Data.List
import GHC.Exts (Constraint)

-- the lattice of Read/Write permissions

data WritePerm = WritePerm deriving Show
data ReadPerm  = ReadPerm deriving Show
data RWPerm    = RWPerm deriving Show

type family SecretImpliesRW (a :: *) (b :: *) :: Constraint where
  SecretImpliesRW RWPerm ReadPerm = ()
  SecretImpliesRW RWPerm WritePerm = ()
  SecretImpliesRW a a  = ()
  SecretImpliesRW a () = ()

class SecretImpliesRW a b => ImpliesRW a b
instance SecretImpliesRW a b => ImpliesRW a b

class Monad m => MonadStateP c s m | m -> s where  
  getp :: (Capability c ImpliesRW, ImpliesRW p ReadPerm) => CapT (c p) m s
  putp :: (Capability c ImpliesRW, ImpliesRW p WritePerm) => s -> CapT (c p) m ()
