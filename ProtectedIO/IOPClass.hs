{-# LANGUAGE GeneralizedNewtypeDeriving,
             DeriveDataTypeable,
             DeriveGeneric,
             MultiParamTypeClasses,
             FlexibleInstances,
             BangPatterns
  #-}

module ProtectedIO.IOPClass (
  IOP (),
  IOPC (),
  runIOP,
  MonadIOP (..),
  module ProtectedIO.Permissions,
) where

import System.IO
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Applicative
import Text.Printf
import Data.Typeable
import EffectCapabilities
import System.IO
import ProtectedIO.Permissions
import {-# SOURCE #-} Config
import GHC.Generics
import Control.Monad.Trans

instance Send ConfigChan IOPC FileInputPerm where
  receive FileInputPerm = return $ IOPC FileInputPerm

instance Send ConfigChan IOPC FileOutputPerm where
  receive FileOutputPerm = return $ IOPC FileOutputPerm

data IOPC a = IOPC a deriving Show

instance Capability IOPC ImpliesIO where
  attenuate (IOPC _) perm = IOPC perm

newtype IOP a = IOP {unIOP :: IO a}
  deriving (Functor, Monad, Typeable, MonadFix, Applicative, HPrintfType, PrintfType)

runIOP :: IOP a -> IO a
runIOP pio = unIOP pio

class Monad m => MonadIOP m where
 liftIOP :: IO a -> CapT (IOPC perm) m a

instance MonadIOP IOP where
 liftIOP io = lift $ IOP io
