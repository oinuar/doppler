{-# LANGUAGE JavaScriptFFI #-}

module Doppler.GHCJS.EvStore (
   EvStore, requireEvStore, delegateEvent
) where

import GHCJS.Types   (JSVal)
import Data.JSString (JSString)

newtype EvStore = EvStore JSVal

foreign import javascript interruptible "require(['ev-store'], $c);"
   requireEvStore :: IO EvStore

foreign import javascript unsafe "if ($3.target !== undefined && $1($3.target)[$2] !== undefined) $1($3.target)[$2]($3);"
   delegateEvent :: EvStore -> JSString -> JSVal -> IO ()
