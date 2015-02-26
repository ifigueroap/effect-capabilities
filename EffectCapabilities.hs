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

  -- * Generic Capabilities Framework

  -- ** Capability Transformer
  CapT (..),
  fromCapT,
  mapCapT,
  withCapability,

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

A CapT k m a denotes a computation 'm a' that requires capability 'c'
to be run.  Here the idea is to exploit monadic computations as an
action to be executed, like is done e.g. in the IO Monad.  Therefore,
any module can construct a computation CapT c m a, requiring
capability c, but only those entities that have access to c can run
these computations, by calling 'fromCapT'.

-}

newtype CapT c m a = CapT (ReaderT c m a) 
        deriving (Functor, Monad, MonadPlus, MonadIO, MonadCont) -- Add MonadTrans if not using monad views

-- This is required because the custom definition of MonadTrans in
-- Monad Views does not work with GeneralizedNewtypeDeriving
instance MonadTrans (CapT c) where
  lift m = CapT . ReaderT $ \_ -> m
                 
-- | Similar to 'mapReaderT', transforms the computation protected by 'CapT'.
mapCapT :: (m a -> n b) -> CapT c m a -> CapT c n b
mapCapT f (CapT c) = CapT (mapReaderT f c)

{- |

Uses capability 'c' to run a computation 'CapT c m a'

This function is strict in the evaluation of 'c' in order to enforce
the integrity of the capability.  Otherwise, a malicious module could
simply pass 'undefined' coerced to the required type.

-}

fromCapT :: c -> CapT c m a -> m a
fromCapT !c (CapT m) = runReaderT m c

withCapability m c = fromCapT c m

instance Monad m => MonadReader c (CapT c m) where
   ask              = CapT ask
   local f (CapT m) = CapT (local f m)

newtype Channel ch a = Channel (Reader ch a) deriving Monad

fromChannel :: ch -> Channel ch a -> a
fromChannel !ch (Channel m) = runReader m ch 

instance MonadReader ch (Channel ch) where
   ask                 = Channel ask
   local f (Channel m) = Channel (local f m)

{- |

We use generic programming to automatically derive the
implementations of 'attenuate'.

-}

class Capability c_p impl | c_p -> impl where
  attenuate :: impl perm perm' => c_p perm -> perm' -> c_p perm'

  default attenuate :: (Generic (c_p perm), Generic (c_p perm'), GGetTag perm' (Rep (c_p perm)) (Rep (c_p perm'))) 
                    => c_p perm -> perm' -> c_p perm'

  attenuate c_p p = to $ gGetTag p (from c_p)

{- |

We also use generic programming to provide default instances for 'Send'.

-}
class Send channel c a where 
   receive  :: a -> Channel channel (c a)

   default receive :: (Generic (c a), GSend a (Rep (c a))) => a -> Channel channel (c a)
   receive a = return $ to (gReceive a)

--- Generic programming boilerplate, to avoid explicit definition of instances

class GGetTag a rep rep' where
  gGetTag :: a -> rep a' -> rep' a'
 
instance GGetTag a (M1 i_1 c_1 (M1 i_2 c_2 (M1 i_3 c_4 (K1 p a'))))
                   (M1 i_1 c_1 (M1 i_2 c_2 (M1 i_3 c_4 (K1 p a))))
         where gGetTag a (M1 (M1 (M1 (K1 a')))) = M1 (M1 (M1 (K1 a)))

class GSend a rep where
  gReceive :: a -> rep b

instance GSend a (M1 i_1 c_1 (M1 i_2 c_2 (M1 i_3 c_4 (K1 p a))))
    where gReceive a = M1 (M1 (M1 (K1 a)))
