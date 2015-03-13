{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module TaggedMonads.GenericStack (
       push,
       pop,
       mkStackHandle,
) where

import Control.Monad.Trans
import Control.Monad.Views
import Control.Monad.Reader
import Control.Monad.MonadStatePV
import Control.Monad.MonadErrorPV
import EffectCapabilities
import Control.Monad.Mask

type StackHandleConstraint m n n' cs s ce permS permE =   (Monad n, Monad n', Monad m,
  MonadStatePV cs [s] n m, MonadErrorPV ce String n' m,
  Capability cs ImpliesRW, ImpliesRW permS ReadPerm, ImpliesRW permS WritePerm,
  Capability ce ImpliesEx, ImpliesEx permE ThrowPerm)

data StackHandle m n n' cs ce ps pe = StackHandle { tagS :: n :><: m,
                                                    tagE :: n' :><: m,
                                                    capS :: cs ps,
                                                    capE :: ce pe }
                                      
mkStackHandle :: (StackHandleConstraint m n n' cs s ce permS permE
  -- Monad n, Monad n', Monad m,
  -- MonadStatePV cs [s] n m, MonadErrorPV ce String n' m,
  -- Capability cs ImpliesRW, ImpliesRW permS ReadPerm, ImpliesRW permS WritePerm,
  -- Capability ce ImpliesEx, ImpliesEx permE ThrowPerm
  ) => n :><: m -> n' :><: m -> cs permS -> ce permE -> StackHandle m n n' cs ce permS permE

mkStackHandle = StackHandle

push :: (StackHandleConstraint m n n' cs s ce permS permE)
        => StackHandle m n n' cs ce permS permE -> s -> m ()
push (StackHandle tagS tagE capS capE) x = do
    stack <- getpv tagS `withCapability` capS
    putpv tagS (x:stack) `withCapability` capS
    return ()

pop :: (StackHandleConstraint m n n' cs s ce permS permE) =>
       StackHandle m n n' cs ce permS permE -> m s
pop (StackHandle tagS tagE capS capE) = do
  stack <- getpv tagS `withCapability` capS
  if null stack
     then throwErrorpv tagE "Stack is empty" `withCapability`capE
     else do putpv tagS (tail stack) `withCapability` capS
             return $ head stack
