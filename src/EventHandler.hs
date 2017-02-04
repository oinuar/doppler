module EventHandler (
   EventHandler, EventHandlerIO
) where

import Event

type EventHandler a s = (Event -> s -> s) -> a
type EventHandlerIO s = EventHandler (IO ()) s
