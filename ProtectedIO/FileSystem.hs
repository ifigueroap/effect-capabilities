{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ProtectedIO.FileSystem where

import System.IO as IO
import ProtectedIO.Permissions
import ProtectedIO.IOPClass (IOP, IOPC, liftIOP)
import EffectCapabilities

{- Standard Handles -}

stdin :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
         => IOPC perm -> Handle
stdin c = checkCapability c `seq` IO.stdin

stdout :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
         => IOPC perm -> Handle
stdout c = checkCapability c `seq` IO.stdout

isEOF :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
         => IOPC perm -> IOP Bool
isEOF c = liftIOP c $ IO.isEOF

getChar :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
         => IOPC perm -> IOP Char
getChar c = liftIOP c $ IO.getChar

putChar :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
         => IOPC perm -> Char -> IOP ()
putChar c car = liftIOP c $ IO.putChar car

getLine :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
         => IOPC perm -> IOP String
getLine c = liftIOP c $ IO.getLine

getContents :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
               => IOPC perm -> IOP String
getContents c = liftIOP c $ IO.getContents

putStr :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
          => IOPC perm -> String -> IOP ()
putStr c s = liftIOP c $ IO.putStr s

readLn :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
          => IOPC perm -> IOP String
readLn c = liftIOP c $ IO.readLn

readIO :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm, Read a)
         => IOPC perm -> String -> IOP a
readIO c a = liftIOP c $ IO.readIO a

print :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm, Show a)
         => IOPC perm -> a -> IOP ()
print c a = liftIOP c $ IO.print a

interact :: (Capability IOPC ImpliesIO, ImpliesIO perm StandardHandlesPerm)
         => IOPC perm -> (String -> String) -> IOP ()
interact c f = liftIOP c $ IO.interact f

{- File Access -}
withFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileAccessPerm)
         => IOPC perm -> FilePath -> IOMode -> (Handle -> IO r) -> IOP r
withFile c path mode f = liftIOP c $ IO.withFile path mode f

openFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileAccessPerm)
         => IOPC perm -> FilePath -> IOMode -> IOP Handle
openFile c path mode = liftIOP c $ IO.openFile path mode

hClose :: (Capability IOPC ImpliesIO, ImpliesIO perm FileAccessPerm)
         => IOPC perm -> Handle -> IOP ()
hClose c h = liftIOP c $ IO.hClose h

withBinaryFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileAccessPerm)
         => IOPC perm -> FilePath -> IOMode -> (Handle -> IO r) -> IOP r
withBinaryFile c path mode f = liftIOP c $ IO.withBinaryFile path mode f

openBinaryFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileAccessPerm)
         => IOPC perm -> FilePath -> IOMode -> IOP Handle
openBinaryFile c path mode = liftIOP c $ IO.openBinaryFile path mode

hSetBinaryMode :: (Capability IOPC ImpliesIO, ImpliesIO perm FileAccessPerm)
         => IOPC perm -> Handle -> Bool -> IOP ()
hSetBinaryMode c h b = liftIOP c $ IO.hSetBinaryMode h b

{- Temp Files -}
openTempFile :: (Capability IOPC ImpliesIO, ImpliesIO perm TempFilesPerm)
             => IOPC perm -> FilePath -> String -> IOP (FilePath, Handle)
openTempFile c p s = liftIOP c $ IO.openTempFile p s

openBinaryTempFile :: (Capability IOPC ImpliesIO, ImpliesIO perm TempFilesPerm)
                   => IOPC perm -> FilePath -> String -> IOP (FilePath, Handle)
openBinaryTempFile c p s = liftIOP c $ IO.openTempFile p s

openTempFileWithDefaultPermissions :: (Capability IOPC ImpliesIO, ImpliesIO perm TempFilesPerm)
                   => IOPC perm -> FilePath -> String -> IOP (FilePath, Handle)
openTempFileWithDefaultPermissions c p s =
  liftIOP c $ IO.openTempFileWithDefaultPermissions p s

openBinaryTempFileWithDefaultPermissions :: (Capability IOPC ImpliesIO, ImpliesIO perm TempFilesPerm)
                  => IOPC perm -> FilePath -> String -> IOP (FilePath, Handle)
openBinaryTempFileWithDefaultPermissions c p s =
  liftIOP c $ IO.openBinaryTempFileWithDefaultPermissions p s


{- File Input -}
readFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileInputPerm)
         => IOPC perm -> FilePath -> IOP String
readFile c path = liftIOP c $ IO.readFile path

{- File Output -}
writeFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileOutputPerm)
         => IOPC perm -> FilePath -> String -> IOP ()
writeFile c path s = liftIOP c $ IO.writeFile path s

appendFile :: (Capability IOPC ImpliesIO, ImpliesIO perm FileOutputPerm)
         => IOPC perm -> FilePath -> String -> IOP ()
appendFile c path s = liftIOP c $ IO.appendFile path s

{- Handle Operations -}

-- Handle Query Ops
hIsEOF :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
         => IOPC perm -> Handle -> IOP Bool
hIsEOF c h = liftIOP c $ IO.hIsEOF h

hIsOpen :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
        => IOPC perm -> Handle -> IOP Bool
hIsOpen c h = liftIOP c $ IO.hIsOpen h

hIsClosed :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
          => IOPC perm -> Handle -> IOP Bool
hIsClosed c h = liftIOP c $ IO.hIsClosed h

hIsReadable :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
            => IOPC perm -> Handle -> IOP Bool
hIsReadable c h = liftIOP c $ IO.hIsReadable h

hIsWritable :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
             => IOPC perm -> Handle -> IOP Bool
hIsWritable c h = liftIOP c $ IO.hIsWritable h

hIsSeekable :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
            => IOPC perm -> Handle -> IOP Bool
hIsSeekable c h = liftIOP c $ IO.hIsSeekable h

hFileSize :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
          => IOPC perm -> Handle -> IOP Integer
hFileSize c h = liftIOP c $ IO.hFileSize h

hGetBuffering :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
              => IOPC perm -> Handle -> IOP BufferMode
hGetBuffering c h = liftIOP c $ IO.hGetBuffering h

hGetPosn :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleQueryOpsPerm)
            => IOPC perm -> Handle -> IOP HandlePosn
hGetPosn c h = liftIOP c $ IO.hGetPosn h

{- Handle Change Ops -}

hSetFileSize :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleChangeOpsPerm)
             => IOPC perm -> Handle -> Integer -> IOP ()
hSetFileSize c h s = liftIOP c $ IO.hSetFileSize h s

hSetBuffering :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleChangeOpsPerm)
              => IOPC perm -> Handle -> BufferMode -> IOP ()
hSetBuffering c h m = liftIOP c $ IO.hSetBuffering h m

hFlush :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleChangeOpsPerm)
       => IOPC perm -> Handle -> IOP ()
hFlush c h = liftIOP c $ IO.hFlush h

hSetPosn :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleChangeOpsPerm)
         => IOPC perm -> HandlePosn -> IOP ()
hSetPosn c pos = liftIOP c $ IO.hSetPosn pos

hSeek :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleChangeOpsPerm)
      => IOPC perm -> Handle -> SeekMode -> Integer -> IOP ()
hSeek c h m i = liftIOP c $ IO.hSeek h m i

hTell :: (Capability IOPC ImpliesIO, ImpliesIO perm HandleChangeOpsPerm)
      => IOPC perm -> Handle -> IOP Integer
hTell c h = liftIOP c $ IO.hTell h


