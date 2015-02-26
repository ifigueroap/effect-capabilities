{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module A where

import EffectCapabilities
import GHC.Generics
import Control.Monad.MonadStateP
import {-# SOURCE #-} B

data RWCap p = RWCap p

instance Capability RWCap ImpliesRW where
  attenuate c_p perm' = RWCap perm'

instance Send BChannel RWCap RWPerm where       
  receive _ = return $ RWCap RWPerm
