{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             DeriveGeneric
  #-}

module C where

import EffectCapabilities
import GHC.Generics
import Control.Monad.MonadStateP
import A
import B


data CChannel = CChannelV 

capV :: RWCap ReadPerm
capV = fromChannel CChannelV $ receive ReadPerm
