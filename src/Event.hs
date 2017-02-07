{-# LANGUAGE JavaScriptFFI #-}

module Event (
   EventHandler, JSEvent, module T, toEvent
) where

import Event.Types as T
import GHCJS.Marshal
import GHCJS.Types
import JavaScript.Object
import JavaScript.Object.Internal   (Object (..))
import Data.Maybe                   (fromJust)
import Control.Applicative          ((<|>))
import Data.JSString                (pack)

newtype JSEvent = JSEvent Event

type Handler s = T.Event -> s -> IO s
type EventHandler a s = Handler s -> a

instance FromJSVal JSEvent where
   fromJSVal val = do
      xs <- sequence [fromFocusEvent val,
                      fromMouseEvent val,
                      fromKeyboardEvent val]

      return $ foldl (<|>) Nothing xs

toEvent :: JSEvent -> Event
toEvent (JSEvent event) =
   event

fromFocusEvent :: JSVal -> IO (Maybe JSEvent)
fromFocusEvent val =
   return $ if isFocusEvent val
      then Just . JSEvent $ FocusEvent
      else Nothing

fromMouseEvent :: JSVal -> IO (Maybe JSEvent)
fromMouseEvent val =
   let precondition = isMouseEvent val
   in fromMouseEvent' precondition $ Object val
   where
      fromMouseEvent' shouldMarshal obj
         | shouldMarshal = do
            screenX <- getProp (pack "screenX") obj
            screenX' <- fromJSVal screenX

            screenY <- getProp (pack "screenY") obj
            screenY' <- fromJSVal screenY

            clientX <- getProp (pack "clientX") obj
            clientX' <- fromJSVal clientX

            clientY <- getProp (pack "clientY") obj
            clientY' <- fromJSVal clientY

            button <- getProp (pack "button") obj
            button' <- fromJSVal button

            buttons <- getProp (pack "buttons") obj
            buttons' <- fromJSVal buttons

            return . Just . JSEvent $ MouseEvent {
               getScreenX = fromJust screenX',
               getScreenY = fromJust screenY',
               getClientX = fromJust clientX',
               getClientY = fromJust clientY',
               getButton = fromJust button',
               getButtons = fromJust buttons'
            }
         | otherwise =
            return Nothing

fromKeyboardEvent :: JSVal -> IO (Maybe JSEvent)
fromKeyboardEvent val =
   let precondition = isKeyboardEvent val
   in fromKeyboardEvent' precondition $ Object val
   where
      fromKeyboardEvent' shouldMarshal obj
         | shouldMarshal = do
            charCode <- getProp (pack "charCode") obj
            charCode' <- fromJSVal charCode

            keyCode <- getProp (pack "keyCode") obj
            keyCode' <- fromJSVal keyCode

            which <- getProp (pack "which") obj
            which' <- fromJSVal which

            location <- getProp (pack "location") obj
            location' <- fromJSVal location

            ctrlKey <- getProp (pack "ctrlKey") obj
            ctrlKey' <- fromJSVal ctrlKey

            altKey <- getProp (pack "altKey") obj
            altKey' <- fromJSVal altKey

            shiftKey <- getProp (pack "shiftKey") obj
            shiftKey' <- fromJSVal shiftKey

            metaKey <- getProp (pack "metaKey") obj
            metaKey' <- fromJSVal metaKey

            return . Just . JSEvent $ KeyboardEvent {
               getCharCode = fromJust charCode',
               getKeyCode = fromJust keyCode',
               getWhich = fromJust which',
               getLocation = fromJust location',
               getKbCtrlKey = fromJust ctrlKey',
               getKbAltKey = fromJust altKey',
               getKbShiftKey = fromJust shiftKey',
               getKbMetaKey = fromJust metaKey'
            }
         | otherwise =
            return Nothing

foreign import javascript unsafe "$1 instanceof FocusEvent"
   isFocusEvent :: JSVal -> Bool

foreign import javascript unsafe "$1 instanceof MouseEvent"
   isMouseEvent :: JSVal -> Bool

foreign import javascript unsafe "$1 instanceof KeyboardEvent"
   isKeyboardEvent :: JSVal -> Bool
