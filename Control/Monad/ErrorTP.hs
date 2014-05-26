{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances,
             GeneralizedNewtypeDeriving,
             FlexibleContexts
  #-}

module Control.Monad.ErrorTP where

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Error
import Control.Monad.RWS.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class
import Control.Monad.MonadStateP
import Control.Monad.MonadErrorP
import EffectCapabilities

{- Observe we don't export ErrorTP, unlike the ErrotT implementation.
   And observe that ErrorTP is not an instance of MonadError
 -}

newtype ErrorTP c e m a = ErrorTP {runETP :: ErrorT e m a} 
        deriving (Functor, Monad, MonadPlus, MonadTrans, MonadCont, MonadIO) 

-- Instance of the protected MonadErrorP class
instance (Monad m, Error e) => MonadErrorP c e (ErrorTP (c ()) e m) where
    throwErrorp     = lift . ErrorTP . throwError
    catchErrorp m h = lift . ErrorTP $ catchError (runETP m) (runETP . h)
    
runErrorTP :: (Error e, Monad m) => ErrorTP c e m a -> m (Either e a)
runErrorTP m = runErrorT (runETP m)

-- -- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (Error e, MonadRWS r w s m) => MonadRWS r w s (ErrorTP c e m)

instance (Error e, MonadStateP c s m) => MonadStateP c s (ErrorTP (c2 ()) e m) where
    getp   = mapCapT lift getp
    putp s = mapCapT lift $ putp s

instance (Error e, MonadState s m) => MonadState s (ErrorTP c e m) where
    get = lift get
    put = lift . put

instance (Error e, MonadError e m) => MonadError e (ErrorTP k e m) where
    throwError       = lift . throwError
    m `catchError` h = ErrorTP $ ErrorT $ runErrorTP m
        `catchError` \e -> runErrorTP (h e)

instance (Error e, MonadReader r m) => MonadReader r (ErrorTP c e m) where
    ask       = lift ask
    local f m = ErrorTP $ ErrorT $ local f (runErrorTP m)

instance (Error e, MonadWriter w m) => MonadWriter w (ErrorTP c e m) where
    tell     = lift . tell
    listen m = ErrorTP $ ErrorT $ do
        (a, w) <- listen (runErrorTP m)
        case a of
            Left  l -> return $ Left  l
            Right r -> return $ Right (r, w)
    pass   m = ErrorTP $ ErrorT $ pass $ do
        a <- runErrorTP m
        case a of
            Left  l      -> return (Left  l, id)
            Right (r, f) -> return (Right r, f)
