{-# LANGUAGE TypeOperators,
             FlexibleContexts,
             FlexibleInstances,
             ScopedTypeVariables,
             NoMonomorphismRestriction,
             MultiParamTypeClasses,
             UndecidableInstances,
             Unsafe
  #-}

module ExampleP where 

import Stack
import QueueP
import qualified PriorityQueueP as PQ
import Control.Monad.Identity
import Control.Monad.MonadErrorP
import Control.Monad.MonadStateP
import Control.Monad.ErrorTP 
import Control.Monad.State.StateTP
import EffectCapabilities
import ProtectedIO.IOPClass
import Config

type M = ErrorTP (QError ()) String (StateTP (QState ()) [Int] IOP)

type N = StateTP (QState ()) [Int] Identity

runN :: N a -> a
runN = runIdentity . flip evalStateTP []

runM :: M a -> IO (Either String a)
runM c = runIOP $ flip evalStateTP [] $ runErrorTP c

data ExampleChan = ExampleChan

{- Handling Queue exceptions examples -}

-- Observe that it is not possible to define a 'catch-all' 
-- handler using the regular catchError function.

dequeueExample1 :: M Int
dequeueExample1 = do enqueue 10
                     x <- dequeueEx
                     y <- dequeueEx -- throws exception
                     return (x+y)


tcQErrorPerm :: QError TCPerm
tcQErrorPerm = fromChannel ExampleChan $ receive TCPerm

catchQErrorPerm :: QError CatchPerm
catchQErrorPerm = attenuate tcQErrorPerm CatchPerm

dequeueExample2 :: M (Maybe Int)
dequeueExample2 = fromCapT catchQErrorPerm $
                           (do {x <- dequeueExample1; return (Just x)})
                           `catchErrorp`
                           (\_ -> return Nothing)

-- Priority Queue example
priorityQueueEx :: M (Maybe Int, Maybe Int)
priorityQueueEx = do enqueue 10
                     enqueue 5
                     enqueue 1
                     maxWord <- PQ.peekByPriority compare
                     minWord <- PQ.peekByPriority compareMin
                     return (maxWord, minWord)
                  where compareMin a b = case (compare a b) of
                          GT -> LT
                          LT -> GT
                          EQ -> EQ

{-
Note that readConfig allows to read arbitrary files.
But if we limit Config to read a concrete name, then
we provide limited IO access to this module.
-}
loadConfigEx = do config <- readConfig "foo.txt"
                  return config

loadConfigEx' = do config <- readConfigDefault
                   return config
