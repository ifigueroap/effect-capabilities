import EffectCapabilities
import Queue
import Stack
import Control.Monad.State.StateTP
import Control.Monad.Identity

type M = StateTP (QState ()) [Int] (StateTP (SState ()) [Int] Identity)

runM c = runIdentity (evalStateTP (evalStateTP c []) [])

client :: M Int
client = do push 1
            enqueue 2
            x <- pop
            y <- pop -- exception, popping from empty stack!
            return (x + y)

runClient = runM client            
