{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ProtectedIO.Permissions (
  -- IO permission lattice  
  ImpliesIO (),

  -- FileSystem permissions
  FileSystemPerm (..),
  StandardHandlesPerm (..),
  FileAccessPerm (..),
  TempFilesPerm (..),
  FileInputPerm (..),
  FileOutputPerm (..),
  HandleOperationsPerm (..),
  HandleQueryOpsPerm (..),
  HandleChangeOpsPerm (..),

  -- HandleIO permissions
  HandleIOPerm (..),
  BinaryIOPerm (..),
  IOInputPerm (..),
  IOOutputPerm (..),
  TextIOPerm (..),
  TextInputPerm (..),
  TextOutputPerm (..),

  -- Encoding permissions
  EncodingPerm (..),
  ConstantsPerm (..), 
  EncodingQueryOpsPerm (..),
  EncodingQueryChangeOpsPerm (..),

  -- Strictness check for IO capabilities
  checkCapability
  ) where

import EffectCapabilities

class SecretImpliesIO a b
class SecretImpliesIO a b => ImpliesIO a b
instance SecretImpliesIO a a 
instance SecretImpliesIO a ()
instance SecretImpliesIO a b => ImpliesIO a b

checkCapability :: a -> a
checkCapability c = case c of _ -> c

-- FileSystem permissions
data FileSystemPerm = FileSystemPerm deriving (Show)
data StandardHandlesPerm = StandardHandlesPerm deriving (Show)
data FileAccessPerm = FileAccessPerm deriving (Show)
data TempFilesPerm = TempFilesPerm deriving (Show)
data FileInputPerm = FileInputPerm deriving (Show)
data FileOutputPerm = FileOutputPerm deriving (Show)
data HandleOperationsPerm = HandleOperationsPerm deriving (Show)
data HandleQueryOpsPerm = HandleQueryOpsPerm deriving (Show)
data HandleChangeOpsPerm = HandleChangeOpsPerm deriving (Show)


-- FileSystem permission lattice
instance SecretImpliesIO FileSystemPerm StandardHandlesPerm
instance SecretImpliesIO FileSystemPerm FileAccessPerm
instance SecretImpliesIO FileSystemPerm FileInputPerm
instance SecretImpliesIO FileSystemPerm FileOutputPerm
instance SecretImpliesIO FileSystemPerm HandleOperationsPerm
instance SecretImpliesIO FileAccessPerm TempFilesPerm
instance SecretImpliesIO HandleOperationsPerm HandleQueryOpsPerm
instance SecretImpliesIO HandleOperationsPerm HandleChangeOpsPerm


-- HandleIO permissions
data HandleIOPerm = HandleIOPerm
data BinaryIOPerm = BinaryIOPerm
data IOInputPerm  = IOInputPerm
data IOOutputPerm = IOOutputPerm
data TextIOPerm = TextIOPerm
data TextInputPerm = TextInputPerm
data TextOutputPerm = TextOutputPerm

-- HandleIO permission lattice
instance SecretImpliesIO HandleIOPerm BinaryIOPerm
instance SecretImpliesIO HandleIOPerm TextIOPerm
instance SecretImpliesIO BinaryIOPerm IOInputPerm
instance SecretImpliesIO BinaryIOPerm IOOutputPerm
instance SecretImpliesIO TextIOPerm TextInputPerm
instance SecretImpliesIO TextIOPerm TextOutputPerm

-- Encoding Permissions
data EncodingPerm = EncodingPerm
data ConstantsPerm = ConstantsPerm
data EncodingQueryOpsPerm = EncodingQueryOpsPerm
data EncodingQueryChangeOpsPerm = EncodingQueryChangeOpsPerm

-- Encoding permission lattice
instance SecretImpliesIO EncodingPerm ConstantsPerm
instance SecretImpliesIO EncodingPerm EncodingQueryOpsPerm
instance SecretImpliesIO EncodingPerm EncodingQueryChangeOpsPerm
