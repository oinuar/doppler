{-# LANGUAGE JavaScriptFFI #-}

module Doppler.GHCJS.DOM (
   DomNode, startEventCapturing, stopEventCapturing, replaceBody
) where

import GHCJS.Foreign.Callback
import GHCJS.Types            (JSVal)
import Data.JSString          (JSString)

newtype DomNode = DomNode JSVal

foreign import javascript unsafe "document.addEventListener($1, $2, true);"
   startEventCapturing :: JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "document.removeEventListener($1, $2, true);"
   stopEventCapturing :: JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "while (document.body.hasChildNodes()) document.body.removeChild(document.body.firstChild); document.body.appendChild($1);"
   replaceBody :: DomNode -> IO ()
