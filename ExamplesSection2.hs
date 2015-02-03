{-# LANGUAGE FlexibleContexts,
             TypeOperators,
             UndecidableInstances,
             FunctionalDependencies,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances
  #-}

module ExamplesSec2 where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

-- Monad stack meant to hold the state of the queue and stack ADTs

type M = StateT [Int] (StateT [Int] Identity)

-- Run function, initializes both states to the empty list
runM :: M a -> a
runM c = runIdentity $ evalStateT (evalStateT c []) []

{- Code from Section 2.2 -}

enqueue1 :: MonadState [Int] m => Int -> m ()
enqueue1 n = do queue <- get
                put $ queue ++ [n]

dequeue1 :: MonadState [Int] m => m Int
dequeue1 = do queue <- get
              put $ tail queue
              return $ head queue

push1 :: MonadState [Int] m => Int -> m ()
push1 n = do stack <- get
             put (n:stack)

pop1 :: MonadState [Int] m => m Int
pop1 = do stack <- get
          put $ tail stack
          return $ head stack

-- Interference happens in client1
client1 :: M Int
client1 = do push1 1
             enqueue1 2    -- value is put into the state layer used by the stack
             x <- pop1
             y <- pop1     -- should raise error because stack should be empty
             return (x+y)

-- Evaluating runM client1 yields 3, but it should raise an error
p1 = runM client1


-- Solution based on explicit lifting

-- The type signature requires the state layer to be in the second
-- layer of the monad stack; thus being tighly coupled to the shape of
-- the monad stack
enqueue1' :: (Monad (t m), MonadTrans t, MonadState [Int] m) => Int -> (t m) ()
enqueue1' n = do queue <- lift get
                 (lift . put) (queue ++ [n])

-- Now there is no interference
client1' :: (Monad (t m), MonadTrans t, MonadState [Int] m, MonadState [Int] (t m)) => (t m) Int
client1' = do push1 1
              enqueue1' 2
              x <- pop1
              y <- pop1     -- error trying to pop twice
              return (x+y)

-- Now, as expected, the error is thrown
p1' = runM client1'


{- Code from Section 2.3: State Encapsulation -}

{-- This goes into a Queue module --}

-- Module Queue (enqueue, dequeue, QueueT ()) where

-- We show the polymorphic implementation of enqueue, dequeue, using MonadQueue

newtype QueueT s m a = QueueT { unQueueT :: StateT [s] m a }
        deriving (Functor, Monad, MonadTrans)

evalQueueT :: Monad m => QueueT s m a -> [s] -> m a
evalQueueT = evalStateT . unQueueT

class Monad m => MonadQueue s m | m -> s where
   enq  :: s -> m ()
   deq  :: m s
   getQ :: m [s] -- auxiliary, to implement dequeueEx below

instance Monad m => MonadQueue s (QueueT s m) where
   enq s = QueueT $ StateT $ \q -> return ((), q ++ [s]) 
   deq   = QueueT $ StateT $ \q -> return (head q, tail q)
   getQ  = QueueT $ StateT $ \q -> return (q, q)

enqueue :: MonadQueue s m => s -> m ()
enqueue = enq

dequeue :: MonadQueue s m => m s
dequeue = deq


{-- This goes into a Stack module --}

-- We show the polymorphic implementation of push, pop, using MonadStack
   
newtype StackT s m a = StackT { unStackT :: StateT [s] m a }
        deriving (Functor, Monad, MonadTrans)

evalStackT :: Monad m => StackT s m a -> [s] -> m a
evalStackT = evalStateT . unStackT

class Monad m => MonadStack s m | m -> s where
   push' :: s -> m ()
   pop'  :: m s

instance Monad m => MonadStack s (StackT s m) where
   push' s = StackT $ StateT $ \st -> return ((), s:st) 
   pop'    = StackT $ StateT $ \st -> return (head st, tail st)

push :: MonadStack s m => s -> m ()
push = push'

pop :: MonadStack s m => m s
pop = pop'


{-- For the following example we need to accomodate implicit lifting --}

instance MonadStack ss m => MonadStack ss (QueueT sq m) where
  push' = lift . push
  pop'  = lift $ pop

instance MonadQueue sq m => MonadQueue sq (StackT ss m) where
  enq   = lift . enq
  deq   = lift $ deq
  getQ  = lift $ getQ

instance MonadError e m => MonadError e (QueueT s m) where
  throwError       = lift . throwError
  m `catchError` h = QueueT $ StateT $ \s -> runStateT (unQueueT m) s
        `catchError` \e -> runStateT (unQueueT (h e)) s

instance MonadError e m => MonadError e (StackT s m) where
  throwError       = lift . throwError
  m `catchError` h = StackT $ StateT $ \s -> runStateT (unStackT m) s
        `catchError` \e -> runStateT (unStackT (h e)) s                           
  

{-- Avoiding interference using MonadQueue and MonadStack --}

type M' = QueueT Int (StackT Int Identity)

runM' :: M' a -> a
runM' c = runIdentity $ evalStackT (evalQueueT c []) []
  
client2 :: M' Int
client2 = do push (1 :: Int)
             enqueue (2 :: Int)
             x <- pop
             y <- pop     -- error: popping from empty stack
             return (x+y)

-- Raises the error as expected
p2 = runM' client2


{- Code of Section 2.4: Exception interference -}

-- Implementation of dequeue that raises an exception
-- We extend MonadQueue with a new getQ to ease this implementation
dequeueEx :: (MonadQueue s m, MonadError String m) => m s
dequeueEx = do queue <- getQ
               if null queue
                  then throwError "Empty queue"
                  else deq

consume :: (MonadQueue Int m, MonadError String m) => m Int
consume = do x <- dequeueEx
             if (x < 0)
                then throwError "Process error"
                else return x

process :: (MonadQueue Int m, MonadError String m) => Int -> m Int
process val = consume `catchError` (\e -> return val)


-- New monad stack for these examples
type M'' = QueueT Int (StackT Int (ErrorT String Identity))

runM'' :: M'' a -> Either String a
runM'' c = runIdentity $ runErrorT $ evalStackT (evalQueueT c []) []

-- runM'' program1 yields 23, as expected
program1 :: M'' Int
program1 = do enqueue (-10)
              process 23

-- runM'' program2 also yields 23, because the inner exception is
-- swallowed. It should propagate the exception
program2 :: M'' Int
program2 = process 23




             
