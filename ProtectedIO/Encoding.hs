{-# LANGUAGE FlexibleContexts
  #-}

module ProtectedIO.Encoding where

import System.IO as IO
import ProtectedIO.Permissions
import ProtectedIO.IOPClass (IOP, IOPC, liftIOP)
import EffectCapabilities

{- Constants -}

-- latin1 :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) ITextEncoding
-- latin1= IO.latin1

-- utf8 :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) ITextEncoding
-- utf8= IO.utf8

-- utf8_bom :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) ITextEncoding
-- utf8_bom= IO.utf8_bom

-- utf16 :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) ITextEncoding
-- utf16= IO.utf16

-- utf161e :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) ITextEncoding
-- utf161e= IO.utf16le

-- utf161be :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) ITextEncoding
-- utf161be= IO.utf16be

-- utf32 :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) ITextEncoding
-- utf32= IO.utf32

-- utf321e :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) ITextEncoding
-- utf321e= IO.utf32le

-- utf321be :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) ITextEncoding
-- utf321be= IO.utf32be

-- localeEncoding :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) ITextEncoding
-- localeEncoding= IO.localeEncoding

-- char8 :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) ITextEncoding
-- char8= IO.char8

-- nativeNewline :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) INewline
-- nativeNewline= IO.nativeNewline

-- noNewlineTranslation :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) INewlineMode
-- noNewlineTranslation= IO.noNewlineTranslation

-- universalNewlineMode :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) INewlineMode
-- universalNewlineMode= IO.universalNewlineMode

-- nativeNewlineMode :: (Capability IOPC perm, ImpliesIO perm ConstantsPerm)
--        => CapT (IOPC perm) INewlineMode
-- nativeNewlineMode= IO.nativeNewlineMode

-- {- Encoding Query Ops -}
-- hGetEncoding :: (Capability IOPC perm, ImpliesIO perm EncodingQueryOpsPerm)
--        => Handle -> CapT (IOPC perm) IIOP (Maybe TextEncoding)
-- hGetEncodingh = liftIOP$ IO.hGetEncoding h

-- {- Encoding Change Ops -}
-- hSetEncoding :: (Capability IOPC perm, ImpliesIO perm EncodingQueryChangeOpsPerm)
--        => Handle -> TextEncoding -> CapT (IOPC perm) IIOP ()
-- hSetEncodingh t = liftIOP$ IO.hSetEncoding h t

-- mkTextEncoding :: (Capability IOPC perm, ImpliesIO perm EncodingQueryChangeOpsPerm)
--        => String -> CapT (IOPC perm) IIOP TextEncoding
-- mkTextEncodings = liftIOP$ IO.mkTextEncoding s

-- hSetNewlineMode :: (Capability IOPC perm, ImpliesIO perm EncodingQueryChangeOpsPerm)
--        => Handle -> NewlineMode -> CapT (IOPC perm) IOP ()
-- hSetNewlineModeh m = liftIOP$ IO.hSetNewlineMode h m






