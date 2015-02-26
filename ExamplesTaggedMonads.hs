module ExamplesTaggedMonads where

import Control.Monad.Mask
import ProtectedIO.IOP as IOP
import Queue

type M = TErrorTP (QError ()) String (TStateTP (QState ()) [Int] IOP)

runM c = runIOP $ evalTStateTP [] $ evalTStateTP [] $ runTErrorTP c

{- Handling Queue exceptions examples -}

-- Observe that it is not possible to define a 'catch-all' 
-- handler using the regular catchError function.

dequeueExample1 :: M Int
dequeueExample1 = do enqueue 10
                     x <- dequeueEx
                     y <- dequeueEx -- throws exception
                     return (x+y)
