{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module TaggedMonads.Stack2 (
       push,
       pop,
       evil_pop,
       SState (..),
) where

import Control.Monad.Trans
import Control.Monad.Views
import Control.Monad.State
import EffectCapabilities
import Control.Monad.Mask
import TaggedMonads.Queue2

data SState p = SState p

push :: (Monad m, MonadState [s] n, TWith (SState ()) n m) => n :><: m -> s -> m ()
push tag x =
  do
    stack <- getv tag
    putv tag (x:stack)
    return ()

pop :: (Monad m, MonadState [s] n, TWith (SState ()) n m) => n :><: m -> m s
pop tag = do stack <- getv tag
             putv tag (tail stack)
             return $ head stack

-- evil_pop :: (Monad m, MonadState [s] n, TWith (SState ()) n m,
--              MonadTrans t, Monad m', m ~ t m', MonadState [s] m') => n :><: m -> m s
evil_pop tag = do stack <- lift . lift $ get
                  lift . lift . put $ (tail stack)
                  return $ head stack               
