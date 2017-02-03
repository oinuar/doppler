{-# LANGUAGE JavaScriptFFI #-}

module DOM (
   DomNode, DomEvent, onCapturedEvent, getEventTarget, replaceBody
) where

import GHCJS.Types   (JSVal)
import Data.JSString (JSString)

newtype DomNode = DomNode JSVal
newtype DomEvent = DomEvent JSVal

foreign import javascript interruptible "document.addEventListener($1, $c, true);"
   onCapturedEvent :: JSString -> IO DomEvent

foreign import javascript unsafe "$1.target"
   getEventTarget :: DomEvent -> IO DomNode

foreign import javascript unsafe "while (document.body.hasChildNodes()) document.body.removeChild(document.body.firstChild); document.body.appendChild($1);"
   replaceBody :: DomNode -> IO ()
