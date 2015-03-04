{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Monad.Zipper where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error.Class
import Control.Monad.Writer
import Control.Monad.MonadStateP
import Control.Monad.MonadErrorP
import EffectCapabilities
import Unsafe.Coerce

newtype (t1 :> (t2 :: (* -> *) -> * -> *))  m a = Z { runZ :: t1 (t2 m) a }

leftL   = runZ
rightL  = Z

instance (MonadTrans t1, MonadTrans t2, Monad m) => Monad ((t1 :> t2) m) where
  return x  = returnZ x
  m >>= f   = m `bindZ` f  

returnZ :: forall a t1 t2 m. (MonadTrans t1, MonadTrans t2, Monad m) => a -> (t1 :> t2) m a
returnZ x = case (mt :: Transformation t2 m) of
              MT -> case (mt :: Transformation t1 (t2 m)) of
                      MT -> Z $ return x

bindZ :: forall a b t1 t2 m. (MonadTrans t1, MonadTrans t2, Monad m) 
      => (t1 :> t2) m a -> (a -> (t1 :> t2) m b) -> (t1 :> t2) m b
m `bindZ` f = case (mt :: Transformation t2 m) of
                MT -> case (mt :: Transformation t1 (t2 m)) of
                        MT -> Z $ runZ m >>= runZ . f 

instance (MonadTrans t1, MonadTrans t2) => MonadTrans (t1 :> t2) where
  lift m  = liftZ m
  mt      = MT
  unlift  = unliftZ

liftZ :: forall a t1 t2 m. (MonadTrans t1, MonadTrans t2, Monad m) => m a -> (t1 :> t2) m a
liftZ m = case (mt :: Transformation t2 m) of
            MT -> Z $ lift $ lift $ m

unliftZ :: forall m n a t1 t2. (Monad m, Monad n, MonadTrans t1, MonadTrans t2) 
        => (forall f. Functor f => (forall x. (t1 :> t2) m x -> m (f x)) -> n (f a)) -> (t1 :> t2) n a
unliftZ f = case (mt :: Transformation t2 m) of
              MT -> case (mt :: Transformation t2 n) of
                      MT -> Z $ unlift $ \ul1 -> unlift $ \ul2 -> liftM runFComp $ f $ \m -> liftM FComp $ ul2 $ ul1 $ runZ $ m

newtype FComp f1 f2 a = FComp { runFComp :: f1 (f2 a) }
instance (Functor f1, Functor f2) => Functor (FComp f1 f2) where
  fmap f = FComp . fmap (fmap f) . runFComp

-- ############# Instances for Protected Transformers ##########################

instance (MonadTrans t1, MonadTrans t2, Monad m, MonadStateP k s (t2 m)) 
      => MonadStateP k s ((t1 :> t2) m) where
  getp = mapCapT (Z . lift) getp 
  putp = mapCapT (Z . lift) . putp

instance (MonadTrans t1, MonadTrans t2, Monad m, MonadErrorP k e (t2 m), MonadErrorP k e (t1 (t2 m)))
         => MonadErrorP k e ((t1 :> t2) m) where
  throwErrorp e = mapCapT (Z . lift) $ throwErrorp e
  catchErrorp m h = mapCapT Z $ runZ m `catchErrorp` (runZ . h)


    
{-

instance MonadErrorP c e m => MonadErrorP c e (Tagged (c ()) m) where
  throwErrorp = mapCapT lift.throwErrorp
  catchErrorp m h = mapCapT lift $ (unTag m `catchErrorp` (\e -> unTag (h e)))

-}

  
-- #############################################################################

instance (MonadTrans t1, MonadTrans t2, Monad m, MonadState s (t2 m)) 
      => MonadState s ((t1 :> t2) m) where
  get   = Z $ lift $ get 
  put s = Z $ lift $ put s  

instance (MonadTrans t1, MonadTrans t2, Monad m, MonadError e (t2 m))
      => MonadError e ((t1 :> t2) m) where
  throwError e   = Z $ lift $ throwError e
  catchError m h = Z $ unlift (\ul -> ul (runZ m) `catchError` (ul . runZ . h))

instance (MonadTrans t1, MonadTrans t2, Monad m, MonadReader e (t2 m)) 
     => MonadReader e ((t1 :> t2) m) where
  ask        = Z $ lift $ ask
  local f m  = Z $ unlift (\ul -> local f (ul $ runZ m))

instance (MonadTrans t1, MonadTrans t2, Monad m, MonadWriter w (t2 m)) 
     => MonadWriter w ((t1 :> t2) m) where
  tell c    = Z $ lift $ tell c
  -- listen = ??
  -- pass   = ??
