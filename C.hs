{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             DeriveGeneric
  #-}

module C where

import EffectCapabilities
import GHC.Generics
import A
import B


data CChannel = CChannelV 

capV :: Cap Perm
capV = fromChannel CChannelV $ receive Perm