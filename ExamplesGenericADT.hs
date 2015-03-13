{-# LANGUAGE MultiParamTypeClasses #-}


module ExampleGenericADT where

import Control.Monad.MonadStatePV
import Control.Monad.MonadErrorPV

import TaggedMonads.GenericStack
import EffectCapabilities

data Q1State p = Q1State p
data Q1Error p = Q1Error p

instance Capability Q1State ImpliesRW where
  attenuate (Q1State _) perm = Q1State perm

instance Capability Q1Error ImpliesEx where
  attenuate (Q1Error _) perm = Q1Error perm

data Q2State p = Q2State p
data Q2Error p = Q2Error p

instance Capability Q2State ImpliesRW where
  attenuate (Q2State _) perm = Q2State perm

instance Capability Q2Error ImpliesEx where
  attenuate (Q2Error _) perm = Q2Error perm

type M = TStateTP (Q1State ()) [Int]
         (TStateTP (Q2State ()) [Bool]
          (TErrorTP (Q1Error ()) String
           (TErrorTP (Q2Error ()) String
            Identity)))

runM :: M a -> Either String (Either String a)
runM = runIdentity . runTErrorTP . runTErrorTP . evalTStateTP [] . evalTStateTP []

program :: M (Int, Bool)
program = do push queue1 10
             x <- pop queue1
             push queue2 True
             y <- pop queue2
             return (x, y)
          where queue1 = mkStackHandle tagQ1S tagQ1E (Q1State RWPerm) (Q1Error TCPerm)
                queue2 = mkStackHandle tagQ2S tagQ2E (Q2State RWPerm) (Q2Error TCPerm)
                tagQ1S = structure (tag :: Q1State ())
                tagQ2S = structure (tag :: Q2State ())
                tagQ1E = structure (tag :: Q1Error ())
                tagQ2E = structure (tag :: Q2Error ())
