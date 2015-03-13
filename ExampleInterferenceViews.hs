{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExamplesTaggedMonads where

import Control.Monad.Identity
import Control.Monad.Mask

import TaggedMonads.Queue2
import TaggedMonads.Stack2

-- ############# Effect interference using explicit lifting #############

type M = TStateT (SState ()) [Int] (TStateT (QState ()) [Int] Identity)

runM :: M a -> a
runM = runIdentity . evalTStateT [] . evalTStateT []

-- Throws an error because using views there is no implicit interference
client1 :: M Int
client1 = do enqueue vq (1 :: Int)
             push vs (2 :: Int)
             x <- pop vs
             y <- pop vs
             return (x+y)
          where vq = structure (QState ())
                vs = structure (SState ())

-- A "wrong" implementation of pop could access the state of the queue
client2 :: M Int
client2 = do enqueue vq (1 :: Int)
             push vs (2 :: Int)
             x <- evil_pop vs
             y <- pop vs
             return (x+y)
          where vq = structure (QState ())
                vs = structure (SState ())

client3 :: M Int
client3 = do enqueue vq (1 :: Int)
             push vs (2 :: Int)
             x <- evil_pop vs
             y <- pop vs
             z <- dequeue vq -- value was consumed by evil_pop
             return (x+y)
          where vq = structure (QState ())
                vs = structure (SState ())
                
