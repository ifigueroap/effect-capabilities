{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module ExamplesTaggedMonads where

import Control.Monad.Identity
import Control.Monad.Mask
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.Views
import Control.Monad.Zipper
import Control.Monad.ErrorTP
import Control.Monad.MonadErrorP
import Control.Monad.MonadErrorPV
import Control.Monad.State.StateTP
import Control.Monad.MonadStatePV
import TaggedMonads.Queue
import TaggedMonads.Stack
import EffectCapabilities

-- ############# Effect interference using explicit lifting #############



-- ############# Examples without interference using protected and tagged monads #############


type M = TStateTP (SState ()) [Int] (TStateTP (QState ()) [Int] Identity)

type N = TStateTP (QState ()) [Int] (TStateTP (SState ()) [Int] Identity)

runM :: M a -> a
runM = runIdentity . evalTStateTP [] . evalTStateTP []

runN :: N a -> a
runN = runIdentity . evalTStateTP [] . evalTStateTP []

-- C-like ADT programming. The views are the handles for the datastructure, and the ADT operations are generic.

-- Must throw an error due to the empty queue, that is, there is no effect interference
client1 :: M Int
client1 = do enqueue vq (1 :: Int)
             push vs (2 :: Int)
             x <- pop vs
             y <- pop vs
             return (x+y)
          where vq = structure (tag :: (QState ()))
                vs = structure (tag :: (SState ()))

-- Must throw an error due to the empty queue, that is, there is no effect interference
client1' :: (MonadStatePV SState [Int] n' m, MonadStatePV QState [Int] n m) => n :><: m -> n' :><: m -> m Int
client1' vq vs = do enqueue vq (1 :: Int)
                    push vs (2 :: Int)
                    x <- pop vs
                    y <- pop vs
                    return (x+y)

client1M :: M Int
client1M  = client1' (structure (tag :: (QState ()))) (structure (tag :: (SState ())))

client1N :: N Int
client1N = client1' (structure (tag :: (QState ()))) (structure (tag :: (SState ())))

client2 :: M Int
client2 = do enqueue vq (1 :: Int)
             push vs (2 :: Int)
             x <- pop vs
             y <- dequeue vq
             return (x+y)
          where vq = structure (tag :: (QState ()))
                vs = structure (tag :: (SState ()))
             
client3 :: M Int
client3 = do push vs (1 :: Int)
             push vs (2 :: Int)
             x <- pop vs
             y <- pop vs
             return (x+y)
          where vs = structure (tag :: (SState ()))

client4 :: M Int
client4 = do enqueue vq (10 :: Int)
             enqueue vq (20 :: Int)
             x <- dequeue vq
             y <- dequeue vq
             return (x+y)
          where vq = structure (tag :: (QState ()))

-- ##################### Examples with exceptions #####################
data EError p = EError p

instance Capability EError ImpliesEx where
  attenuate (EError _) perm = EError perm
  
type L = TErrorTP (QError ()) String (TErrorTP (EError ()) String (TStateTP (QState ()) [Int] Identity))

runL :: L a -> Either String (Either String a)
runL = runIdentity . evalTStateTP [] . runTErrorTP . runTErrorTP

-- receives (QError TCPerm) capability
data ExampleChan = ExampleChan

tcQErrorPerm :: QError TCPerm
tcQErrorPerm = fromChannel ExampleChan $ receive TCPerm

-- Using attenuate
catchQErrorPerm :: QError CatchPerm
catchQErrorPerm = attenuate tcQErrorPerm CatchPerm

-- Example of protected exceptions
consume :: (
  MonadStatePV QState [Int]  n   m,
  MonadErrorPV QError String n'  m,
  MonadErrorPV EError String n'' m) => n :><: m -> n' :><: m -> n'' :><: m -> m Int
consume vqs vqe vee =  do x <- dequeueEx vqs vqe
                          if (x < 0)
                            then throwErrorpv vee "Process error" `withCapability` EError ThrowPerm
                            else return x           

client5 :: L Int
client5 = consume vqs vqe vee
          where vqs = structure (tag :: QState ())
                vqe = structure (tag :: QError ())
                vee = structure (tag :: EError ())


client6 :: L Int
client6 = do enqueue vqs 10
             consume vqs vqe vee
          where vqs = structure (tag :: QState ())
                vqe = structure (tag :: QError ())
                vee = structure (tag :: EError ())

client7 :: L Int
client7 = do enqueue vqs (-10)
             consume vqs vqe vee
          where vqs = structure (tag :: QState ())
                vqe = structure (tag :: QError ())
                vee = structure (tag :: EError ())

-- -- cannot lift catchErrorp due to limitation in mtl
-- -- the tranformers library has the liftCatch operation
-- -- This is why we use the regular ErrorT transformer in addition to ErrorTP
             
process :: Int -> L Int
process val = catchErrorpv vee (consume vqs vqe vee) (\e -> return val) `withCapability` EError CatchPerm
  where vqs = structure (tag :: QState ())
        vqe = structure (tag :: QError ())
        vee = structure (tag :: EError ())

client8 :: L Int
client8 = do process 23
             enqueue vqs (10)
             x <- dequeueEx vqs vqe
             return x
          where vqs = structure (tag :: QState ())
                vqe = structure (tag :: QError ())             

-- Uses default value 23, because it catches process-invariant exception (values > 0)
program1 :: L Int
program1 = do enqueue vqs (-10)
              process 23
           where vqs = structure (tag :: QState ())

-- Raises "Queue is empty" exception, because it is not caught by process error handler
program2 :: L Int
program2 = process 23

-- Catches queue-invariant from program2, and runs program1 with no issues
debug :: L Int -> L Int
debug prog = catchErrorpv vqe prog (\e -> error "Error in queue invariant!") `withCapability` catchQErrorPerm
             where vqe = structure (tag :: QError ())
