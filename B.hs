{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             DeriveGeneric
  #-}

module B where

import EffectCapabilities
import GHC.Generics
import Control.Monad.MonadStateP
import {-# SOURCE #-} C
import {-# SOURCE #-} A

data BChannel = BChannelV

instance Send CChannel RWCap ReadPerm where
   receive ReadPerm = return (attenuate rwCap ReadPerm)
     where rwCap = fromChannel BChannelV $ receive RWPerm

-- This fails because A does not send a capabitiliy with permissiom Perm to B
-- instance Send CChannel Cap SomePerm where
--          receive CChannelV SomePermV = (receive BChannelV SomePermV :: Cap SomePerm)
