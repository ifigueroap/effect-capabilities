{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances
  #-}

module A where

import EffectCapabilities

data RWCap p = RWCap p