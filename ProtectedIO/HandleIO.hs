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
        => IOPC perm -> Handle -> Ptr a -> Int -> IOP Int
hGetBuf c h p i = liftIOP c $ IO.hGetBuf h p i

hGetBufSome :: (Capability IOPC ImpliesIO, ImpliesIO perm IOInputPerm)
            => IOPC perm -> Handle -> Ptr a -> Int -> IOP Int
hGetBufSome c h p i = liftIOP c $ IO.hGetBufSome h p i

hGetBufNonBlocking :: (Capability IOPC ImpliesIO, ImpliesIO perm IOInputPerm)
                   => IOPC perm -> Handle -> Ptr a -> Int -> IOP Int
hGetBufNonBlocking c h p i = liftIOP c $ IO.hGetBufNonBlocking h p i

{- IO Output -}
hPutBuf :: (Capability IOPC ImpliesIO, ImpliesIO perm IOOutputPerm)
        => IOPC perm -> Handle -> Ptr a -> Int -> IOP ()
hPutBuf c h p i = liftIOP c $ IO.hPutBuf h p i

hPutBufNonBlocking :: (Capability IOPC ImpliesIO, ImpliesIO perm IOOutputPerm)
        => IOPC perm -> Handle -> Ptr a -> Int -> IOP Int
hPutBufNonBlocking c h p i = liftIOP c $ IO.hPutBufNonBlocking h p i


{- TextIO -}

{- Text Input -}

hWaitForInput :: (Capability IOPC ImpliesIO, ImpliesIO perm TextInputPerm)
              => IOPC perm -> Handle -> Int -> IOP Bool
hWaitForInput c h i = liftIOP c $ IO.hWaitForInput h i

hReady :: (Capability IOPC ImpliesIO, ImpliesIO perm TextInputPerm)
              => IOPC perm -> Handle -> IOP Bool
hReady c h = liftIOP c $ IO.hReady h

hGetChar :: (Capability IOPC ImpliesIO, ImpliesIO perm TextInputPerm)
         => IOPC perm -> Handle -> IOP Char
hGetChar c h = liftIOP c $ IO.hGetChar h

hGetLine :: (Capability IOPC ImpliesIO, ImpliesIO perm TextInputPerm)
         => IOPC perm -> Handle -> IOP String
hGetLine c h = liftIOP c $ IO.hGetLine h

hLookAhead :: (Capability IOPC ImpliesIO, ImpliesIO perm TextInputPerm)
           => IOPC perm -> Handle -> IOP Char
hLookAhead c h = liftIOP c $ IO.hLookAhead h

hGetContents :: (Capability IOPC ImpliesIO, ImpliesIO perm TextInputPerm)
             => IOPC perm -> Handle -> IOP String
hGetContents c h = liftIOP c $ IO.hGetContents h

{- Text Output -}

hPutChar :: (Capability IOPC ImpliesIO, ImpliesIO perm TextOutputPerm)
         => IOPC perm -> Handle -> Char -> IOP ()
hPutChar c h char = liftIOP c $ IO.hPutChar h char

hPutStr :: (Capability IOPC ImpliesIO, ImpliesIO perm TextOutputPerm)
        => IOPC perm -> Handle -> String -> IOP ()
hPutStr c h str = liftIOP c $ IO.hPutStr h str

hPutStrLn :: (Capability IOPC ImpliesIO, ImpliesIO perm TextOutputPerm)
          => IOPC perm -> Handle -> String -> IOP ()
hPutStrLn c h str = liftIOP c $ IO.hPutStrLn h str

hPrint :: (Capability IOPC ImpliesIO, ImpliesIO perm TextOutputPerm, Show a)
       => IOPC perm -> Handle -> a -> IOP ()
hPrint c h a = liftIOP c $ IO.hPrint h a
