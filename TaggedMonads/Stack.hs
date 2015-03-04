{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module TaggedMonads.Stack (
       push,
       pop,
       -- popEx,
       SState (),
       -- SError (),
) where

import Control.Monad.Trans
import Control.Monad.Views
import Control.Monad.MonadStatePV
import Control.Monad.MonadErrorPV
import EffectCapabilities
import Control.Monad.Mask

data SState p = SState p

instance Capability SState ImpliesRW where
 attenuate (SState _) perm = SState perm

push :: (MonadStatePV SState [s] n m) => n :><: m -> s -> m ()
push tag x =
  do
    stack <- getpv tag `withCapability` SState ReadPerm
    putpv tag (x:stack) `withCapability` SState WritePerm
    return ()

pop :: (MonadStatePV SState [s] n m) => n :><: m -> m s
pop tag = do stack <- getpv tag `withCapability` SState ReadPerm
             putpv tag (tail stack) `withCapability` SState WritePerm
             return $ head stack

-- data SError p = SError p

-- instance Capability SError ImpliesEx where
--  attenuate (SError _) perm = SError perm     

-- popEx :: forall n n' m s. (
--   MonadStatePV SState n m [s], TWith (SState ()) n m,
--   MonadErrorPV SError n' m String, TWith (SError ()) n' m) => m s
-- popEx = let ?n = undefined :: n () in
--   do stack <- getpv `withCapability` SState ReadPerm
--      if null stack
--        then let ?n = undefined :: n' () in throwErrorpv "Queue is empty" `withCapability` (SError ThrowPerm)
--        else do putpv (tail stack) `withCapability` SState WritePerm
--                return $ head stack     
