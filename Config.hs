module Config (
  ConfigChan (),
  readConfig,
  writeConfig,
  readConfigDefault,
) where

import EffectCapabilities
import GHC.Generics
import ProtectedIO.IOP as IOP

data ConfigChan = ConfigChan

fileInputPerm :: IOPC FileInputPerm
fileInputPerm = fromChannel ConfigChan $ receive FileInputPerm

fileOutputPerm :: IOPC FileOutputPerm
fileOutputPerm = fromChannel ConfigChan $ receive FileOutputPerm

readConfig  f   = fromCapT fileInputPerm $ IOP.readFile f
writeConfig f s = fromCapT fileOutputPerm $ IOP.writeFile f s
readConfigDefault = readConfig "defaultConfig.txt"
