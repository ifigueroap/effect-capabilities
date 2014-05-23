{-# LANGUAGE TypeOperators,
             FlexibleContexts,
             FlexibleInstances,
             ScopedTypeVariables,
             FunctionalDependencies,
             DeriveGeneric
  #-}

module Stack (push, pop, SState ()) where

import MonadStatePV
import EffectCapabilities
import GHC.Generics

newtype SState a = SState a deriving Generic
instance Capability SState ImpliesRW

push :: MonadStatePV SState n m [s] => s -> m ()
push x = do xs <- fromCapT (SState RWPerm) get
            fromCapT (SState WritePerm) $ put (x:xs) 

pop :: MonadStatePV SState n m [s] => m s
pop = do xs <- fromCapT (SState ReadPerm) get
         fromCapT (SState WritePerm) $ put (tail xs)
         return (head xs)