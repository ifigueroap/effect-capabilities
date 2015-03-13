{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

-- There is a bug when using Deriving Generic and hs-boot files in GHC <= 7.6.3
-- https://ghc.haskell.org/trac/ghc/ticket/7878

-- Implementation of the Queue module, considering all enhancements
-- from Section 4.

module TaggedMonads.Queue2 (
       enqueue,
       dequeue,
       QState (..),
) where

import Control.Monad.Trans
import Control.Monad.Mask
import Control.Monad.Views
import Control.Monad.State
import EffectCapabilities

data QState p = QState p

enqueue :: (Monad m, MonadState [s] n, TWith (QState ()) n m) => n :><: m -> s -> m ()
enqueue tag x = do
  queue  <- getv tag
  putv tag (queue ++ [x])
                  
dequeue :: (Monad m, MonadState [s] n, TWith (QState ()) n m) => n :><: m -> m s
dequeue tag = do
  queue <- getv tag
  putv tag (tail queue)
  return $ head queue
