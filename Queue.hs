{-# LANGUAGE TypeOperators,
             FlexibleContexts,
             ScopedTypeVariables,
             DefaultSignatures,
             MultiParamTypeClasses,
             DeriveGeneric,
             FlexibleInstances
  #-}

-- Careful with Deriving Generic and hs-boot files in GHC <= 7.6.3
-- https://ghc.haskell.org/trac/ghc/ticket/7878

module QueueP (
       enqueue,
       dequeue,
       dequeueEx,
       dequeueErr,
       QState (),
       QError ()
) where

import Control.Monad.MonadErrorP
import Control.Monad.MonadStateP
import EffectCapabilities
import GHC.Generics
import {-# SOURCE #-} PriorityQueueP
import {-# SOURCE #-} ExampleP

data QState a = QState a

instance Capability QState ImpliesRW where
 attenuate (QState _) perm = QState perm       

instance Send PQueueChan QState ReadPerm where
 receive perm = return $ QState perm

data QError a = QError a

instance Capability QError ImpliesEx where
 attenuate (QError _) perm = QError perm

instance Send ExampleChan QError TCPerm where
 receive perm = return $ QError perm

enqueue :: MonadStateP QState [s] m => s -> m ()
enqueue x = do queue  <- fromCapT (QState RWPerm) getp
               fromCapT (QState RWPerm) $ putp (queue ++ [x])

dequeue :: MonadStateP QState [s] m => m s
dequeue = do queue <- fromCapT (QState RWPerm) getp
             fromCapT (QState RWPerm) $ putp (tail queue)
             return $ head queue

dequeueErr :: (MonadStateP QState [s] m, MonadErrorP QError String m) => m s
dequeueErr = fromCapT (QError CatchPerm) $ catchErrorp dequeueEx error
                    
dequeueEx :: (MonadStateP QState [s] m, MonadErrorP QError String m) => m s
dequeueEx = do queue <- fromCapT (QState ReadPerm) getp
               if null queue
                  then fromCapT (QError ThrowPerm) $ throwErrorp "Queue is empty"
                  else do fromCapT (QState WritePerm) $ putp (tail queue)
                          return $ head queue
