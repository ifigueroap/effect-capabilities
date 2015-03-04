{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module TaggedMonads.GenericStack (
       push,
       pop,
) where

import Control.Monad.Trans
import Control.Monad.Views
import Control.Monad.Reader
import Control.Monad.MonadStatePV
import EffectCapabilities
import Control.Monad.Mask

push :: (MonadStatePV c [s] n m, ImpliesRW perm ReadPerm, ImpliesRW perm WritePerm)
        => n :><: m -> c perm -> s -> m ()
push tag c_p x = do
    stack <- getpv tag `withCapability` c_p
    putpv tag (x:stack) `withCapability` c_p
    return ()

pop :: (MonadStatePV c [s] n m, ImpliesRW perm ReadPerm, ImpliesRW perm WritePerm) =>
       n :><: m -> c perm -> m s
pop tag c_p = do stack <- getpv tag `withCapability` c_p
                 putpv tag (tail stack) `withCapability` c_p
                 return $ head stack
