{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances,
             GeneralizedNewtypeDeriving,
             FlexibleContexts,
             ScopedTypeVariables,
             InstanceSigs,
             ScopedTypeVariables
  #-}

{-# LANGUAGE ConstraintKinds #-}

module Control.Monad.State.StateTP (
   StateTP (), -- observe we don't export the data constructor
               -- the only way to access the state is using MonadStateP
               -- or using runStateTP with the proper key   
   ReadPerm (..), WritePerm (..), RWPerm (..),
   runStateTP,
   evalStateTP,
   execStateTP,
   -- mapStateTP,
   -- withStateTP,
 ) where

import Control.Monad.State
import Control.Monad.MonadStateP
import EffectCapabilities
 
import Control.Monad.Error.Class
import Control.Monad.Cont.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class
import Control.Monad.MonadErrorP

{- Observe we don't export StateTP, unlike the StateT implementation.
   And observe that StateTP is not an instance of MonadState
 -}

newtype StateTP k s m a = StateTP {runSTP :: StateT s m a} 
        deriving (Functor, Monad, MonadPlus, MonadCont, MonadIO) -- Add MonadTrans if not using MonadViews

instance MonadTrans (StateTP k s) where
   lift m = StateTP . StateT $ \s -> do
        a <- m
        return (a, s)
   mt = MT
   unlift f = StateTP . StateT $ \s -> f (\m -> runStateT (runSTP m) s >>= return . toRP) >>= return . fromRP

data RPair s a = RPair a s
instance Functor (RPair s) where
  fmap f (RPair a s) = RPair (f a) s

toRP   (a,s)       = RPair a s
fromRP (RPair a s) = (a,s)   

runStateTP :: Monad m => StateTP k s m a -> s -> m (a, s)
runStateTP m s = runStateT (runSTP m) s

-- |Similar to 'evalStateT'
evalStateTP :: Monad m => StateTP k s m a -> s -> m a
evalStateTP m s = evalStateT (runSTP m) s

-- |Similar to 'execStateT'
execStateTP :: Monad m => StateTP k s m a -> s -> m s
execStateTP m s = execStateT (runSTP m) s

instance Monad m => MonadStateP k s (StateTP (k ()) s m) where
    getp   = lift . StateTP $ get 
    putp   = lift . StateTP . put 

-- -- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (MonadError e m) => MonadError e (StateTP k s m) where
    throwError       = lift . throwError
    m `catchError` h = StateTP $ StateT $ \s -> runStateT (runSTP m) s
        `catchError` \e -> runStateT (runSTP (h e)) s

-- instance (MonadErrorP c1 e m, Capability c1 p) => MonadErrorP c1 e (StateTP (c ()) s m) where
--     throwErrorp = mapCapT lift . throwErrorp    
    -- catchErrorp m h = mapCapT (StateTP . StateT) $ (runStateT (runSTP m)) `catchErrorp` (runStateT . runSTP . h)
    
-- Needs -fallow-undecidable-instances
instance (MonadReader r m) => MonadReader r (StateTP k s m) where
    ask       = lift ask
    local f m = StateTP $ StateT $ \s -> local f (runStateT (runSTP m) s)

-- Needs -fallow-undecidable-instances
instance (MonadWriter w m, ImpliesRW RWPerm a, Capability k ImpliesRW) => MonadWriter w (StateTP (k a) s m) where
    tell     = lift . tell
    listen m = StateTP $ StateT $ \s -> do
        ~((a, s'), w) <- listen (runStateT (runSTP m) s)
        return ((a, w), s')
    pass   m = StateTP $ StateT $ \s -> pass $ do
        ~((a, f), s') <- runStateT (runSTP m) s
        return ((a, s'), f)
