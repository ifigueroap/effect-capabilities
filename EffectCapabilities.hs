{-# LANGUAGE MultiParamTypeClasses,
             DefaultSignatures,
             FlexibleContexts,
             FlexibleInstances,
             UndecidableInstances,
             KindSignatures,    
             GeneralizedNewtypeDeriving,
             BangPatterns,
             ConstraintKinds,
             FunctionalDependencies
 #-}

module EffectCapabilities (

  -- * Capabilities

  -- ** Capability Transformer
  CapT (..),
  fromCapT,
  mapCapT,

  -- ** Capabilities and Permissions
  Capability (..), 

  -- * Capability Sharing
  Channel,
  fromChannel,
  Send (..)       
) where

import GHC.Generics
import Control.Monad.Reader
import Control.Monad.Cont.Class

{-|

The Capability transformer 'CapT'.

A CapT k m a denotes a computation 'm a' that requires capability 'k'
to be run.  Here the idea is to exploit monadic computations as an
action to be executed, like is done e.g. in the IO Monad.  Therefore,
any module can construct a computation CapT k m a, requiring
capability k, but only those entities that have access to k can run
these computations, by calling 'fromCapT'.

-}

newtype CapT k m a = CapT (ReaderT k m a) 
        deriving (Functor, Monad, MonadTrans, MonadPlus, MonadIO, MonadCont)

                 
-- | Similar to 'mapReaderT', transforms the computation protected by 'CapT'.
mapCapT :: (m a -> n b) -> CapT k m a -> CapT k n b
mapCapT f (CapT c) = CapT (mapReaderT f c)

{- |

Uses capability k to run a computation CapT k m a

This function is strict in the evaluation of k in order to enforce the
integrity of the capability.  Otherwise, a malicious module could
simply pass 'undefined' coerced to the required type.

-}

fromCapT :: k -> CapT k m a -> m a
fromCapT !k (CapT c) = runReaderT c k 

instance Monad m => MonadReader k (CapT k m) where
   ask = CapT ask
   local f (CapT m) = CapT (local f m)

newtype Channel k a = Channel (Reader k a) deriving Monad

fromChannel :: k -> Channel k a -> a
fromChannel !k (Channel c) = runReader c k 

instance MonadReader k (Channel k) where
   ask = Channel ask
   local f (Channel m) = Channel (local f m)


class Capability c_p impl | c_p -> impl where
  attenuate :: impl perm perm' => c_p perm -> perm' -> c_p perm'

  default attenuate :: (Generic (c_p perm), Generic (c_p perm'), GGetTag perm' (Rep (c_p perm)) (Rep (c_p perm'))) 
                    => c_p perm -> perm' -> c_p perm'

  attenuate c_p p = to $ gGetTag p (from c_p)

class Send channel c a where 
   receive  :: a -> Channel channel (c a)

   default receive :: (Generic (c a), GSend a (Rep (c a))) => a -> Channel channel (c a)
   receive a = return $ to (gReceive a)

--- Generic programming, to avoid explicit definition of instances

class GGetTag a rep rep' where
  gGetTag :: a -> rep a' -> rep' a'
 
instance GGetTag a (M1 i_1 c_1 (M1 i_2 c_2 (M1 i_3 c_4 (K1 p a'))))
                   (M1 i_1 c_1 (M1 i_2 c_2 (M1 i_3 c_4 (K1 p a))))
         where gGetTag a (M1 (M1 (M1 (K1 a')))) = M1 (M1 (M1 (K1 a)))

class GSend a rep where
  gReceive :: a -> rep b

instance GSend a (M1 i_1 c_1 (M1 i_2 c_2 (M1 i_3 c_4 (K1 p a))))
    where gReceive a = M1 (M1 (M1 (K1 a)))

-- class Functor' c_p where 
--  fmap' :: (a -> b) -> c_p a -> c_p b        

--  default fmap' :: (Generic (c_p a), Generic (c_p b), GFunctor a b (Rep (c_p a)) (Rep (c_p b)))
--           => (a -> b) -> c_p a -> c_p b        
--  fmap' f a = to $ gFmap f (from a) 

-- class GFunctor a b rep rep' where
--   gFmap :: (a -> b) -> rep a' -> rep' b'

-- instance GFunctor a b (M1 i_1 c_1 (M1 i_2 c_2 (M1 i_3 c_4 (K1 p a)))) (M1 i_1 c_1 (M1 i_2 c_2 (M1 i_3 c_4 (K1 p b)))) 
--     where gFmap f (M1 (M1 (M1 (K1 a)))) = (M1 (M1 (M1 (K1 (f a)))))
