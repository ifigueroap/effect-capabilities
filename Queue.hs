{-# LANGUAGE TypeOperators,
             FlexibleContexts,
             ScopedTypeVariables,
             DefaultSignatures,
             MultiParamTypeClasses,
             DeriveGeneric,
             FlexibleInstances
  #-}

module Queue (
       enqueue,
       dequeue,
       dequeueEx,
       dequeueErr,
       QState (),
       QError ()
) where

import Control.Monad.Error
import MonadStatePV
import MonadErrorPV as EPV
import Control.Monad.Error as E
import EffectCapabilities
import GHC.Generics
import {-# SOURCE #-} PriorityQueue
import {-# SOURCE #-} Example


data QError a = QError a deriving Generic
instance Capability QError ImpliesEx

instance Send ExampleChan QError TCPerm

newtype QState a = QState a
instance Capability QState ImpliesRW
  where attenuate (QState _) perm = QState perm

instance Send PQueueChan QState ReadPerm 
  where receive perm = return $ QState perm  

-- newtype QState a = QState a deriving (Generic, Show)
-- instance Capability QState a
-- instance Send PQueueChan QState ReadPerm

enqueue :: MonadStatePV QState n m [s] => s -> m ()
enqueue x = do queue  <- fromCapT (QState RWPerm) get
               fromCapT (QState RWPerm) $ put (queue ++ [x])

dequeue :: MonadStatePV QState n m [s] => m s
dequeue = do queue <- fromCapT (QState RWPerm) get
             fromCapT (QState RWPerm) $ put (tail queue)
             return $ head queue

dequeueErr :: (MonadStatePV QState n m [s], MonadErrorPV QError n' m String) => m s
dequeueErr = fromCapT (QError CatchPerm) $ EPV.catchError dequeueEx (\e -> error e) 
                    

dequeueEx :: (MonadStatePV QState n m [s], MonadErrorPV QError n' m String) => m s
dequeueEx = do queue <- fromCapT (QState ReadPerm) get
               if null queue
                  then fromCapT (QError ThrowPerm) $ EPV.throwError "Queue is empty"
                  else do fromCapT (QState WritePerm) $ put (tail queue)
                          return $ head queue
