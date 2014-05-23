{-# LANGUAGE FlexibleContexts
  #-}

module ProtectedIO.HandleIO where

import System.IO as IO
import ProtectedIO.Permissions
import ProtectedIO.IOPClass (IOP, IOPC, liftIOP)
import EffectCapabilities
import GHC.Ptr

{- Binary IO -}

{- IO Input -}

hGetBuf :: (Capability IOPC ImpliesIO, ImpliesIO perm IOInputPerm)
        => Handle -> Ptr a -> Int -> CapT (IOPC perm)IOP Int
hGetBuf h p i = liftIOP $ IO.hGetBuf h p i

hGetBufSome :: (Capability IOPC ImpliesIO, ImpliesIO perm IOInputPerm)
            => Handle -> Ptr a -> Int -> CapT (IOPC perm)IOP Int
hGetBufSome h p i = liftIOP $ IO.hGetBufSome h p i

hGetBufNonBlocking :: (Capability IOPC ImpliesIO, ImpliesIO perm IOInputPerm)
                   => Handle -> Ptr a -> Int -> CapT (IOPC perm)IOP Int
hGetBufNonBlocking h p i = liftIOP $ IO.hGetBufNonBlocking h p i

{- IO Output -}
hPutBuf :: (Capability IOPC ImpliesIO, ImpliesIO perm IOOutputPerm)
        => Handle -> Ptr a -> Int -> CapT (IOPC perm)IOP ()
hPutBuf h p i = liftIOP $ IO.hPutBuf h p i

hPutBufNonBlocking :: (Capability IOPC ImpliesIO, ImpliesIO perm IOOutputPerm)
        => Handle -> Ptr a -> Int -> CapT (IOPC perm) IOP Int
hPutBufNonBlocking h p i = liftIOP $ IO.hPutBufNonBlocking h p i


{- TextIO -}

{- Text Input -}

hWaitForInput :: (Capability IOPC ImpliesIO, ImpliesIO perm TextInputPerm)
              => Handle -> Int -> CapT (IOPC perm) IOP Bool
hWaitForInput h i = liftIOP $ IO.hWaitForInput h i

hReady :: (Capability IOPC ImpliesIO, ImpliesIO perm TextInputPerm)
              => Handle -> CapT (IOPC perm) IOP Bool
hReady h = liftIOP $ IO.hReady h

hGetChar :: (Capability IOPC ImpliesIO, ImpliesIO perm TextInputPerm)
         => Handle -> CapT (IOPC perm) IOP Char
hGetChar h = liftIOP $ IO.hGetChar h

hGetLine :: (Capability IOPC ImpliesIO, ImpliesIO perm TextInputPerm)
         => Handle -> CapT (IOPC perm) IOP String
hGetLine h = liftIOP $ IO.hGetLine h

hLookAhead :: (Capability IOPC ImpliesIO, ImpliesIO perm TextInputPerm)
           => Handle -> CapT (IOPC perm) IOP Char
hLookAhead h = liftIOP $ IO.hLookAhead h

hGetContents :: (Capability IOPC ImpliesIO, ImpliesIO perm TextInputPerm)
             => Handle -> CapT (IOPC perm)IOP String
hGetContents h = liftIOP $ IO.hGetContents h

{- Text Output -}

hPutChar :: (Capability IOPC ImpliesIO, ImpliesIO perm TextOutputPerm)
         => Handle -> Char -> CapT (IOPC perm)IOP ()
hPutChar h char = liftIOP $ IO.hPutChar h char

hPutStr :: (Capability IOPC ImpliesIO, ImpliesIO perm TextOutputPerm)
        => Handle -> String -> CapT (IOPC perm)IOP ()
hPutStr h str = liftIOP $ IO.hPutStr h str

hPutStrLn :: (Capability IOPC ImpliesIO, ImpliesIO perm TextOutputPerm)
          => Handle -> String -> CapT (IOPC perm)IOP ()
hPutStrLn h str = liftIOP $ IO.hPutStrLn h str

hPrint :: (Capability IOPC ImpliesIO, ImpliesIO perm TextOutputPerm, Show a)
       => Handle -> a -> CapT (IOPC perm)IOP ()
hPrint h a = liftIOP $ IO.hPrint h a
