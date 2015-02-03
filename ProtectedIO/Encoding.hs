{-# LANGUAGE FlexibleContexts
  #-}

module ProtectedIO.Encoding where

import System.IO as IO
import ProtectedIO.Permissions
import ProtectedIO.IOPClass (IOP, IOPC, liftIOP)
import EffectCapabilities

{- Constants -}

latin1 :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> TextEncoding
latin1 c = checkCapability c `seq` IO.latin1

utf8 :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> TextEncoding
utf8 c = checkCapability c `seq` IO.utf8

utf8_bom :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> TextEncoding
utf8_bom c = checkCapability c `seq` IO.utf8_bom

utf16 :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> TextEncoding
utf16 c = checkCapability c `seq` IO.utf16

utf161e :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> TextEncoding
utf161e c = checkCapability c `seq` IO.utf16le

utf161be :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> TextEncoding
utf161be c = checkCapability c `seq` IO.utf16be

utf32 :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> TextEncoding
utf32 c = checkCapability c `seq` IO.utf32

utf321e :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> TextEncoding
utf321e c = checkCapability c `seq` IO.utf32le

utf321be :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> TextEncoding
utf321be c = checkCapability c `seq` IO.utf32be

localeEncoding :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> TextEncoding
localeEncoding c = checkCapability c `seq` IO.localeEncoding

char8 :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> TextEncoding
char8 c = checkCapability c `seq` IO.char8

nativeNewline :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> Newline
nativeNewline c = checkCapability c `seq` IO.nativeNewline

noNewlineTranslation :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> NewlineMode
noNewlineTranslation c = checkCapability c `seq` IO.noNewlineTranslation

universalNewlineMode :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> NewlineMode
universalNewlineMode c = checkCapability c `seq` IO.universalNewlineMode

nativeNewlineMode :: (Capability IOPC ImpliesIO, ImpliesIO perm ConstantsPerm)
       => IOPC perm -> NewlineMode
nativeNewlineMode c = checkCapability c `seq` IO.nativeNewlineMode

{- Encoding Query Ops -}
hGetEncoding :: (Capability IOPC ImpliesIO, ImpliesIO perm EncodingQueryOpsPerm)
       => IOPC perm -> Handle -> IOP (Maybe TextEncoding)
hGetEncoding c h = liftIOP c $ IO.hGetEncoding h

{- Encoding Change Ops -}
hSetEncoding :: (Capability IOPC ImpliesIO, ImpliesIO perm EncodingQueryChangeOpsPerm)
       => IOPC perm -> Handle -> TextEncoding -> IOP ()
hSetEncoding c h t = liftIOP c $ IO.hSetEncoding h t

mkTextEncoding :: (Capability IOPC ImpliesIO, ImpliesIO perm EncodingQueryChangeOpsPerm)
       => IOPC perm -> String -> IOP TextEncoding
mkTextEncoding c s = liftIOP c $ IO.mkTextEncoding s

hSetNewlineMode :: (Capability IOPC ImpliesIO, ImpliesIO perm EncodingQueryChangeOpsPerm)
       => IOPC perm -> Handle -> NewlineMode -> IOP ()
hSetNewlineMode c h m = liftIOP c $ IO.hSetNewlineMode h m






