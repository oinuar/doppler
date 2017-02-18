module Event.Types (
   Event (..)
) where

import Data.Word
import Data.Int

data Event =
     GenericEvent
   | FocusEvent
   | InputEvent {
      getValue :: String
   }
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
      getScreenX :: Int32,
      getScreenY :: Int32,
      getClientX :: Int32,
      getClientY :: Int32,
      getButton :: Word16,
      getButtons :: Word16 }
   deriving (Eq, Show)
