module Config (
  ConfigChan (),
  readConfig,
  writeConfig,
  readConfigDefault,
) where

import EffectCapabilities
import GHC.Generics
import ProtectedIO.IOP as IOP
import ProtectedIO.Permissions
import System.FilePath

data ConfigChan = ConfigChan

readPerm :: IOPC FileInputPerm
readPerm = fromChannel ConfigChan $ receive FileInputPerm

writePerm :: IOPC FileOutputPerm
writePerm = fromChannel ConfigChan $ receive FileOutputPerm

readConfig  f   = IOP.readFile  readPerm  f
writeConfig f s = IOP.writeFile writePerm f s

readConfigDefault = readConfig "defaultConfig.txt"
