To use regular mtl instead of Monad Views, keep only the following under the Control directory:

- the Control/Monad and Control/Monad/State folders
- Control/Monad/State/StateTP.hs
- ErrorTP.hs
- MonadErrorP.hs
- MonadStateP.hs

All the extra files are due to the implementation of monad views and can be removed. However, they are necessary for running the examples in the file ExamplesTaggedMonads.hs.


