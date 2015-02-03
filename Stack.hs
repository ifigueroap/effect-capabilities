{-# LANGUAGE TypeOperators,
             FlexibleContexts,
             ScopedTypeVariables,
             DefaultSignatures,
             MultiParamTypeClasses,
             DeriveGeneric,
             FlexibleInstances
  #-}

-- There is a bug when using Deriving Generic and hs-boot files in GHC <= 7.6.3
-- https://ghc.haskell.org/trac/ghc/ticket/7878

module Stack (
       push,
       pop,
       SState (),
) where

import Control.Monad.MonadErrorP
import Control.Monad.MonadStateP
import Control.Monad.Trans
import EffectCapabilities
import GHC.Generics


data SState p = SState p -- deriving Generic

instance Capability SState ImpliesRW where
 attenuate (SState _) perm = SState perm

push :: (MonadTrans t, Monad (t m), MonadStateP SState [s] m) => s -> (t m) ()
push x = do stack <- lift $ fromCapT (SState ReadPerm) getp
            lift (fromCapT (SState WritePerm) $ putp (x:stack))

pop :: (MonadTrans t, Monad (t m), MonadStateP SState [s] m) => (t m) s
pop = do stack <- lift $ fromCapT (SState ReadPerm) getp
         lift (fromCapT (SState WritePerm) $ putp (tail stack))
         return $ head stack                    
