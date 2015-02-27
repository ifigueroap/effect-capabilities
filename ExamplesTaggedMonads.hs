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
import Control.Monad.Trans
import Control.Monad.Views
import Control.Monad.Zipper
import Control.Monad.State.StateTP
import Control.Monad.ErrorTP
import Control.Monad.MonadStatePV
import Control.Monad.MonadErrorPV
import EffectCapabilities
import TaggedMonads.Queue
import TaggedMonads.Stack

type M = TStateTP (QState ()) [Int] (TStateTP (SState ()) [Int] Identity)

runM :: M a -> a
runM c = runIdentity $ evalTStateTP [] $ evalTStateTP [] c

-- Must throw an error due to the empty queue, that is, there is no effect interference
client1 :: M Int
client1 = do enqueue (1 :: Int)
             push (2 :: Int)
             x <- pop
             y <- pop
             return (x+y)

client2 :: M Int
client2 = do enqueue (1 :: Int)
             push (2 :: Int)
             x <- pop
             y <- dequeue
             return (x+y)
             
client3 :: M Int
client3 = do push (1 :: Int)
             push (2 :: Int)
             x <- pop
             y <- pop
             return (x+y)

client4 :: M Int
client4 = do enqueue (10 :: Int)
             enqueue (20 :: Int)
             x <- dequeue
             y <- dequeue
             return (x+y)

-- ##################### Examples with exceptions #####################

type N = TErrorTP (QError ()) String (TErrorTP (EError ()) String (TStateTP (QState ()) [Int] Identity))

runN :: N a -> Either String (Either String a)
runN c = runIdentity $ evalTStateTP [] $ runTErrorTP $ runTErrorTP c

-- receives (QError TCPerm) capability
data ExampleChan = ExampleChan

-- tcQErrorPerm :: QError TCPerm
-- tcQErrorPerm = fromChannel ExampleChan $ receive TCPerm

-- -- Using attenuate
-- catchQErrorPerm :: QError CatchPerm
-- catchQErrorPerm = attenuate tcQErrorPerm CatchPerm

data EError p = EError p

instance Capability EError ImpliesEx where
  attenuate (EError _) perm = EError perm

-- Example of protected exceptions
consume :: forall qs qe ee m.(
  MonadStatePV QState qs  m [Int],  TWith (QState ()) qs  m,
  MonadErrorPV QError qe  m String, TWith (QError ()) qe  m,
  MonadErrorPV EError ee  m String, TWith (EError ()) ee  m) => m Int
consume =  do
    x <- let ?qs = undefined :: qs ()
             ?qe = undefined :: qe ()
         in dequeueEx
    if (x < 0)
      then let ?n = undefined :: ee () in throwErrorpv "Process error" `withCapability` EError ThrowPerm
      else return x

client5 :: N Int
client5 = consume

client6 :: N Int
client6 = do enqueue 10
             consume

client7 :: N Int
client7 = do enqueue (-10)
             consume

-- cannot lift catchErrorp due to limitation in mtl
-- the tranformers library has the liftCatch operation
-- This is why we use the regular ErrorT transformer in addition to ErrorTP
             
process :: Int -> N Int
process val = let ?n = undefined in catchErrorpv consume (\e -> return val) `withCapability` EError CatchPerm

client8 :: N Int
client8 = do process 23
             enqueue (10)
             x <- dequeueEx             
             return x
             

-- -- Uses default value 23, because it catches process-invariant exception (values > 0)
-- program1 :: N Int
-- program1 = do enqueue (-10)
--               process 23

-- -- Raises "Queue is empty" exception, because it is not caught by process error handler
-- program2 :: N Int
-- program2 = process 23

-- -- Catches queue-invariant from program2, and runs program1 with no issues
-- debug prog = prog `catchErrorp` (\e -> error "Error in queue invariant!")
--              `withCapability` catchQErrorPerm
