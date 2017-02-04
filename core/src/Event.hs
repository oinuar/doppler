module Event (
   Event (..)
) where

import Data.Word
import Data.Int

data Event =
     FocusEvent
   | FormEvent
   | ViewEvent
   | InputEvent
   | KeyboardEvent {
      getCharCode :: Word,
      getKeyCode :: Word,
      getWhich :: Word,
      getLocation :: Float,
      getKbCtrlKey :: Bool,
      getKbShiftKey :: Bool,
      getKbAltKey :: Bool,
      getKbMetaKey :: Bool }
   | MouseEvent {
      screenX :: Int64,
      screenY :: Int64,
      clientX :: Int64,
      clientY :: Int64,
      button :: Word16,
      buttons :: Word16 }
   deriving (Eq)
