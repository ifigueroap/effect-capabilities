{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

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
import EffectCapabilities
import System.IO
import ProtectedIO.Permissions
import {-# SOURCE #-} Config
import GHC.Generics

instance Send ConfigChan IOPC FileInputPerm
  where receive perm = return $ IOPC perm
        
instance Send ConfigChan IOPC FileOutputPerm
  where receive perm = return $ IOPC perm

data IOPC a = IOPC a
instance Capability IOPC ImpliesIO where
  attenuate (IOPC _) perm = IOPC perm

newtype IOP a = IOP {unIOP :: IO a}
  deriving (Functor, Monad, MonadFix, Applicative, PrintfType, HPrintfType)

runIOP :: IOP a -> IO a
runIOP pio = unIOP pio

class Monad m => MonadIOP m where
  liftIOP :: IOPC perm -> IO a -> m a

instance MonadIOP IOP where
  liftIOP !c io = IOP io
