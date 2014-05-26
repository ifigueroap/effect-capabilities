{-# LANGUAGE TypeOperators,
             FlexibleContexts,
             FlexibleInstances,
             ScopedTypeVariables,
             MultiParamTypeClasses,
             UndecidableInstances
  #-}

module ExamplesSection4 where 

import Queue
import qualified PriorityQueue as PQ
import Control.Monad.Identity
import Control.Monad.MonadErrorP
import Control.Monad.Error
import Control.Monad.MonadStateP
import Control.Monad.ErrorTP 
import Control.Monad.State.StateTP
import EffectCapabilities

type M = ErrorTP (QError ()) String (StateTP (QState ()) [Int] (ErrorT String Identity))

runM :: M a -> Either String (Either String a)
runM c = runIdentity $ runErrorT $flip evalStateTP [] (runErrorTP c)
                     
-- runM c = runIdentity $ flip evalStateTP [] $ runErrorTP (runErrorT c)

-- instance MonadError e m => MonadError e (StateTP c s m) where
--   throwError = lift . throwError

-- instance (Monad m, Error e, MonadStateP c s m) => MonadStateP c s (ErrorT e m) where
--   getp   = lift $ fromCapT (undefined :: (c ReadPerm)) getp
--   putp s = fromCapT (undefined :: (c WritePerm)) putp s

-- instance (Error e1, Error e2, MonadErrorP c e1 m) => MonadErrorP c e1 (ErrorT e2 m) where

-- Section 4.2: PriorityQueue example
priorityQueueEx :: M (Maybe Int, Maybe Int)
priorityQueueEx = do enqueue 10
                     enqueue 5
                     enqueue 1
                     maxWord <- PQ.peekBy compare
                     minWord <- PQ.peekBy compareMin
                     return (maxWord, minWord)
                  where compareMin a b = case (compare a b) of
                          GT -> LT
                          LT -> GT
                          EQ -> EQ

-- Section 4.3: Protected exceptions examples

-- receives (QError TCPerm) capability
data ExampleChan = ExampleChan

tcQErrorPerm :: QError TCPerm
tcQErrorPerm = fromChannel ExampleChan $ receive TCPerm

-- Using attenuate
catchQErrorPerm :: QError CatchPerm
catchQErrorPerm = attenuate tcQErrorPerm CatchPerm

-- Example of protected exceptions

consume :: M Int
consume = do x <- dequeueEx
             if (x < 0)
                then (lift . lift . throwError) "Process error"
                else return x

process :: Int -> M Int
process val = consume `catchError` (\e -> return val)

-- Uses default value 23, because it catches process-invariant exception (values > 0)
program1 :: M Int
program1 = do enqueue (-10)
              process 23

-- Raises "Queue is empty" exception, because it is not caught by process error handler
program2 :: M Int
program2 = process 23

-- Catches queue-invariant from program2, and runs program1 with no issues
debug prog = fromCapT catchQErrorPerm $ prog `catchErrorp` (\e -> error "Error in queue invariant!")

