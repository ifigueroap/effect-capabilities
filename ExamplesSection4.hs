{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module ExamplesSection4 where 

import Queue
import qualified PriorityQueue as PQ
import Control.Monad.Identity
import Control.Monad.MonadErrorP
import Control.Monad.Error
import Control.Monad.ErrorTP 
import Control.Monad.State.StateTP
import EffectCapabilities

type M = ErrorTP  (QError ()) String
         (StateTP (QState ()) [Int]
         (ErrorT  String Identity))

runM :: M a -> Either String (Either String a)
runM c = runIdentity $ runErrorT $flip evalStateTP [] (runErrorTP c)

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
                then throwError "Process error"
                else return x

-- cannot lift catchErrorp due to limitation in mtl
-- the tranformers library has the liftCatch operation
-- This is why we use the regular ErrorT transformer in addition to ErrorTP
             
process :: Int -> M Int
process val =  consume `catchError` (\e -> return val)

-- Uses default value 23, because it catches process-invariant exception (values > 0)
program1 :: M Int
program1 = do enqueue (-10)
              process 23

-- Raises "Queue is empty" exception, because it is not caught by process error handler
program2 :: M Int
program2 = process 23

-- Catches queue-invariant from program2, and runs program1 with no issues
debug prog = prog `catchErrorp` (\e -> error "Error in queue invariant!")
             `withCapability` catchQErrorPerm
