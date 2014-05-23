{-# LANGUAGE FlexibleContexts,
             TypeOperators,
             ScopedTypeVariables,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances
  #-}

module ExamplesSec2 where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Views
import Control.Monad.Mask

type M = StateT [Int] (StateT [Int] Identity)

runM :: M a -> a
runM c = runIdentity $ evalStateT (evalStateT c []) []

{- Take 0: Plain Monadic Programming -}

push, enqueue, enqueue' :: Int -> M ()
pop, dequeue, dequeue'  :: M Int

push n = do stack <- get
            put (n:stack)
pop    = do stack <- get
            let x = head stack
            put (tail stack)
            return x

enqueue n = do queue <- get
               put (queue ++ [n])
dequeue   = do queue <- get
               let x = head queue
               put (tail queue)
               return x

-- Returns 3.
c0 = do push 1
        push 2
        x <- pop
        y <- pop
        return (x+y)

-- Returns 3, incorrectly.
c1 = do push 1
        enqueue 2
        x <- pop
        y <- pop
        return (x+y)

-- Error trying to get an element from the empty list.
c2 = do push 1
        enqueue 2
        x <- pop
        y <- pop
        z <- dequeue
        return (x+y+z)

-- Fixing enqueue and dequeue with explicit lifting
enqueue' n = do queue <- lift get
                (lift . put) (queue ++ [n])
dequeue'   = do queue <- lift get
                let x = head queue
                (lift . put) (tail queue)
                return x

{- Take 1: State Encapsulation -}

newtype QueueT s m a = QueueT { unQState :: StateT [s] m a }
        deriving (Functor, Monad, MonadTrans)

getQueueT :: Monad m => QueueT s m [s]
getQueueT = QueueT $ StateT $ \ s -> return (s,s)

putQueueT :: Monad m => [s] -> QueueT s m ()
putQueueT s' = QueueT $ StateT $ \ _ -> return ((), s')

enqueueQT :: Monad m => s -> QueueT s m ()
enqueueQT s = do queue <- getQueueT
                 putQueueT (queue ++ [s])

dequeueQT :: Monad m => QueueT s m s
dequeueQT = do queue <- getQueueT               
               putQueueT (tail queue)
               return (head queue)

class Monad m => MonadQueue s m where
   enqueueQ :: s -> m ()
   dequeueQ :: m s

instance Monad m => MonadQueue s (QueueT s m) where
   enqueueQ = enqueueQT
   dequeueQ = dequeueQT


newtype StackT s m a = SStateT { unSState :: StateT [s] m a }
        deriving (Functor, Monad, MonadTrans)
            

{- Take 2: Polymorphism -}

push1 :: MonadState [Int] m => Int -> m ()
push1 n = do stack <- get
             put (n:stack)

pop1 :: MonadState [Int] m => m Int
pop1 = do stack <- get
          let x = head stack
          put (tail stack)
          return x

enqueue1 :: MonadState [Int] m => Int -> m ()
enqueue1 n = do queue <- get
                put (queue ++ [n])

dequeue1 :: MonadState [Int] m => m Int
dequeue1 = do queue <- get
              let x = head queue
              put (tail queue)
              return x

-- The same problem is present. It incorrectly returns 3.
c1' :: M Int
c1' = do push1 1
         enqueue1 2
         x <- pop1
         y <- pop1
         return (x+y)

-- enqueue1' :: (Monad (t m), MonadTrans t, MonadState [Int] m) => Int -> (t m) ()
enqueue1' n = do queue <- lift get
                 (lift . put) (queue ++ [n])

c1'' :: (Monad (t m), MonadTrans t, MonadState [Int] m,
         MonadState [Int] (t m)) => (t m) Int
c1'' = do push1 10
          enqueue1' 20
          x <- pop1
          y <- pop1 -- error trying to pop twice
          return (x+y)

{- Take 3: Monad Views -}

data Stack = Stack
data Queue = Queue

type M' = TStateT Stack [Int] (TStateT Queue [Int] Identity)
runM' c = runIdentity $ evalTStateT [] $ evalTStateT [] c

push2 :: forall n m. (TWith Stack n m, MonadState [Int] n) => Int -> m ()
push2 n = do stack <- getv (structure Stack :: n :><: m) :: m [Int]
             putv (structure Stack :: n :><: m) (n:stack)

pop2 :: forall n m. (TWith Stack n m, MonadState [Int] n) => m Int
pop2 = do stack <- getv (structure Stack :: n :><: m) :: m [Int]
          let x = head stack
          putv (structure Stack :: n :><: m) (tail stack)
          return x

enqueue2 :: forall n m. (TWith Queue n m, MonadState [Int] n) => Int -> m ()
enqueue2 n = do queue <- getv (structure Queue :: n :><: m) :: m [Int]
                putv (structure Queue :: n :><: m) (queue ++ [n])

dequeue2 :: forall n m. (TWith Queue n m, MonadState [Int] n) => m Int
dequeue2 = do queue <- getv (structure Queue :: n :><: m) :: m [Int]
              let x = head queue
              putv (structure Queue :: n :><: m) (tail queue)
              return x

c3 :: M' Int
c3 = do push2 10
        enqueue2 20
        x <- pop2
        y <- pop2 -- error trying to pop twice
        return (x+y)


enqueue2' :: forall n n' m.
          (TWith Queue n  m, MonadState [Int] n,
           TWith Stack n' m, MonadState [Int] n') => Int -> m ()
enqueue2' n = do queue <- getv (structure Queue :: n :><: m) :: m [Int]
                 putv (structure Queue :: n :><: m) (queue ++ [n])
                 stack <- getv (structure Stack :: n' :><: m) :: m [Int]
                 putv (structure Stack :: n' :><: m) (reverse stack)

c4 :: M' Int
c4 = do push2 3
        push2 2
        push2 1
        x <- pop2
        enqueue2' 1
        y <- pop2
        return (x+y)

-- Explicit lifting is still possible

-- Second, we can use explicit lifting, omitting tags altogether.
push2' :: (Monad m, MonadTrans t1, MonadTrans t2, MonadTrans t3, MonadTrans t4,
           Monad (t2 (t3 (t4 m))),
           MonadState [Int] (t1 (t2 (t3 (t4 m)))),
           MonadState [Int] (t3 (t4 m)))
           => Int -> (t1 (t2 (t3 (t4 m)))) ()
push2' n = do stack <- get
              put (n:stack)
              queue <- (lift . lift) get
              (lift . lift . put) (queue ++ [n])

c5 :: M' Int
c5 = do push2' 10
        x <- dequeue2 -- push2' put the value into the queue state
        return x