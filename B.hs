{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             DeriveGeneric
  #-}

module B where

import EffectCapabilities
import GHC.Generics
import {-# SOURCE #-} C
import {-# SOURCE #-} A

data BChannel = BChannelV

instance Send CChannel Cap Perm where
         receive Perm = return $ (fromChannel BChannelV $ receive Perm)

-- This fails because A does not send a capabitiliy with permissiom Perm to B
-- instance Send CChannel Cap SomePerm where
--          receive CChannelV SomePermV = (receive BChannelV SomePermV :: Cap SomePerm)