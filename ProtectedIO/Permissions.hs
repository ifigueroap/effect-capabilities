{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             UndecidableInstances
  #-}

module ProtectedIO.Permissions where

class SecretImpliesIO a b
class SecretImpliesIO a b => ImpliesIO a b
instance SecretImpliesIO a a 
instance SecretImpliesIO a b => ImpliesIO a b 


-- FileSystem permissions
data FileSystemPerm = FileSystemPerm
data StandardHandlesPerm = StandardHandlesPerm
data FileAccessPerm = FileAccessPerm
data TempFilesPerm = TempFilesPerm
data FileInputPerm = FileInputPerm
data FileOutputPerm = FileOutputPerm
data HandleOperationsPerm = HandleOperationsPerm
data HandleQueryOpsPerm = HandleQueryOpsPerm
data HandleChangeOpsPerm = HandleChangeOpsPerm

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