module Event (
   EventHandler, EventHandlerIO, runEvent
) where

import Control.Concurrent.STM

type EventHandler a s = (s -> s) -> a
type EventHandlerIO s = EventHandler (IO ()) s

runEvent :: TVar s -> EventHandlerIO s
runEvent var handler =
   atomically $ modifyTVar var handler
