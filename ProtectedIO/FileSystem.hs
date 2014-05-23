{-# LANGUAGE FlexibleContexts
  #-}

module ProtectedIO.FileSystem where

import System.IO as IO
import ProtectedIO.Permissions
import ProtectedIO.IOPClass (IOP, IOPC, liftIOP)
import EffectCapabilities
import Control.Monad.Trans
import Control.Monad.Identity

{- Standard Handles -}

-- stdin :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
--          => CapT (IOPC perm) IOPC Handle
-- stdin = liftIOP $ IO.stdin

-- stdout :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
--          => CapT (IOPC perm) IOP Handle
-- stdout = liftIOP $ IO.stdout

isEOF :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
         => CapT (IOPC perm) IOP Bool
isEOF = liftIOP $ IO.isEOF

getChar :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
         => CapT (IOPC perm) IOP Char
getChar = liftIOP $ IO.getChar

putChar :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
         => Char -> CapT (IOPC perm) IOP ()
putChar car = liftIOP $ IO.putChar car

getLine :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
         => CapT (IOPC perm) IOP String
getLine = liftIOP $ IO.getLine

getContents :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
               => CapT (IOPC perm) IOP String
getContents = liftIOP $ IO.getContents

putStr :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
          => String -> CapT (IOPC perm) IOP ()
putStr s = liftIOP $ IO.putStr s

readLn :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
          => CapT (IOPC perm) IOP String
readLn = liftIOP $ IO.readLn

readIO :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm, Read a)
         => String -> CapT (IOPC perm) IOP a
readIO a = liftIOP $ IO.readIO a

print :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm, Show a)
         => a -> CapT (IOPC perm) IOP ()
print a = liftIOP $ IO.print a

interact :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
         => (String -> String) -> CapT (IOPC perm) IOP ()
interact f = liftIOP $ IO.interact f

{- File Access -}
withFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileAccessPerm)
         => FilePath -> IOMode -> (Handle -> IO r) -> CapT (IOPC perm) IOP r
withFile path mode f = liftIOP $ IO.withFile path mode f

openFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileAccessPerm)
         => FilePath -> IOMode -> CapT (IOPC perm) IOP Handle
openFile path mode = liftIOP $ IO.openFile path mode

hClose :: (Capability IOPC ImpliesIO, ImpliesIO perm FileAccessPerm)
         => Handle -> CapT (IOPC perm) IOP ()
hClose h = liftIOP $ IO.hClose h

withBinaryFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileAccessPerm)
         => FilePath -> IOMode -> (Handle -> IO r) -> CapT (IOPC perm) IOP r
withBinaryFile path mode f = liftIOP $ IO.withBinaryFile path mode f

openBinaryFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileAccessPerm)
         => FilePath -> IOMode -> CapT (IOPC perm) IOP Handle
openBinaryFile path mode = liftIOP $ IO.openBinaryFile path mode

hSetBinaryMode :: (Capability IOPC ImpliesIO, ImpliesIO perm FileAccessPerm)
         => Handle -> Bool -> CapT (IOPC perm) IOP ()
hSetBinaryMode h b = liftIOP $ IO.hSetBinaryMode h b

{- Temp Files -}
openTempFile :: (Capability IOPC ImpliesIO, ImpliesIO perm TempFilesPerm)
             => FilePath -> String -> CapT (IOPC perm) IOP (FilePath, Handle)
openTempFile p s = liftIOP $ IO.openTempFile p s

openBinaryTempFile :: (Capability IOPC ImpliesIO, ImpliesIO perm TempFilesPerm)
                   => FilePath -> String -> CapT (IOPC perm) IOP (FilePath, Handle)
openBinaryTempFile p s = liftIOP $ IO.openTempFile p s

openTempFileWithDefaultPermissions :: (Capability IOPC ImpliesIO, ImpliesIO perm TempFilesPerm)
                   => FilePath -> String -> CapT (IOPC perm) IOP (FilePath, Handle)
openTempFileWithDefaultPermissions p s =
  liftIOP $ IO.openTempFileWithDefaultPermissions p s

openBinaryTempFileWithDefaultPermissions :: (Capability IOPC ImpliesIO, ImpliesIO perm TempFilesPerm)
                  => FilePath -> String -> CapT (IOPC perm) IOP (FilePath, Handle)
openBinaryTempFileWithDefaultPermissions p s =
  liftIOP $ IO.openBinaryTempFileWithDefaultPermissions p s


{- File Input -}
readFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileInputPerm)
         => FilePath -> CapT (IOPC perm) IOP String
readFile path = liftIOP $ IO.readFile path

{- File Output -}
writeFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileOutputPerm)
         => FilePath -> String -> CapT (IOPC perm) IOP ()
writeFile path s = liftIOP $ IO.writeFile path s

appendFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileOutputPerm)
         => FilePath -> String -> CapT (IOPC perm) IOP ()
appendFile path s = liftIOP $ IO.appendFile path s

{- Handle Operations -}

-- Handle Query Ops
hIsEOF :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
         => Handle -> CapT (IOPC perm) IOP Bool
hIsEOF h = liftIOP $ IO.hIsEOF h

hIsOpen :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
        => Handle -> CapT (IOPC perm) IOP Bool
hIsOpen h = liftIOP $ IO.hIsOpen h

hIsClosed :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
          => Handle -> CapT (IOPC perm) IOP Bool
hIsClosed h = liftIOP $ IO.hIsClosed h

hIsReadable :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
            => Handle -> CapT (IOPC perm) IOP Bool
hIsReadable h = liftIOP $ IO.hIsReadable h

hIsWritable :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
             => Handle -> CapT (IOPC perm) IOP Bool
hIsWritable h = liftIOP $ IO.hIsWritable h

hIsSeekable :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
            => Handle -> CapT (IOPC perm) IOP Bool
hIsSeekable h = liftIOP $ IO.hIsSeekable h

hFileSize :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
          => Handle -> CapT (IOPC perm) IOP Integer
hFileSize h = liftIOP $ IO.hFileSize h

hGetBuffering :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
              => Handle -> CapT (IOPC perm) IOP BufferMode
hGetBuffering h = liftIOP $ IO.hGetBuffering h

hGetPosn :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
            => Handle -> CapT (IOPC perm) IOP HandlePosn
hGetPosn h = liftIOP $ IO.hGetPosn h

{- Handle Change Ops -}

hSetFileSize :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleChangeOpsPerm)
             => Handle -> Integer -> CapT (IOPC perm) IOP ()
hSetFileSize h s = liftIOP $ IO.hSetFileSize h s

hSetBuffering :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleChangeOpsPerm)
              => Handle -> BufferMode -> CapT (IOPC perm) IOP ()
hSetBuffering h m = liftIOP $ IO.hSetBuffering h m

hFlush :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleChangeOpsPerm)
       => Handle -> CapT (IOPC perm) IOP ()
hFlush h = liftIOP $ IO.hFlush h

hSetPosn :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleChangeOpsPerm)
         => HandlePosn -> CapT (IOPC perm) IOP ()
hSetPosn pos = liftIOP $ IO.hSetPosn pos

hSeek :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleChangeOpsPerm)
      => Handle -> SeekMode -> Integer -> CapT (IOPC perm) IOP ()
hSeek h m i = liftIOP $ IO.hSeek h m i

hTell :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleChangeOpsPerm)
      => Handle -> CapT (IOPC perm) IOP Integer
hTell h = liftIOP $ IO.hTell h


