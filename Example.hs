{-# LANGUAGE TypeOperators,
             FlexibleContexts,
             FlexibleInstances,
             ScopedTypeVariables,
             NoMonomorphismRestriction,
             MultiParamTypeClasses,
             UndecidableInstances,
             Unsafe
  #-}

module Example where 

import Stack
import Queue
import qualified PriorityQueue as PQ

import Control.Monad.Identity
import Control.Monad.Mask
import Control.Monad.Error as E
import EffectCapabilities
import MonadStatePV
import MonadErrorPV as EPV
import ProtectedIO.IOPClass
import Config

type M = TErrorTP (QError ()) String (TStateTP (QState ()) [Int] IOP)

-- type M = ErrorTP (QError ()) String (StateTP (QState ()) [Int] IOP)


runM c = runIOP $ evalTStateTP [] $ evalTStateTP [] $ runTErrorTP c

{- Handling Queue exceptions examples -}

-- Observe that it is not possible to define a 'catch-all' 
-- handler using the regular catchError function.

dequeueExample1 :: M Int
dequeueExample1 = do enqueue 10
                     x <- dequeueEx
                     y <- dequeueEx -- throws exception
                     return (x+y)

data ExampleChan = ExampleChan

tcQErrorPerm :: QError TCPerm
tcQErrorPerm = fromChannel ExampleChan $ receive TCPerm

catchQErrorPerm :: QError CatchPerm
catchQErrorPerm = attenuate tcQErrorPerm CatchPerm

dequeueExample2 :: M (Maybe Int)
dequeueExample2 = fromCapT catchQErrorPerm $
                          EPV.catchError (do x <- dequeueExample1
                                             return (Just x))
                                         (\e -> return Nothing)
                         

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
