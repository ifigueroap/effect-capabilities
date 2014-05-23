{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,             
             DeriveGeneric
  #-}

module A where

import EffectCapabilities
import GHC.Generics
import {-# SOURCE #-} B

data Cap a = Cap a 
data Perm = Perm 

instance Send BChannel Cap Perm where       
  receive _ = return $ Cap Perm
