{-# LANGUAGE JavaScriptFFI #-}

module EvStore (
   EvStore, requireEvStore, dispatchEvent
) where

import DOM           (DomNode)
import GHCJS.Types   (JSVal)
import Data.JSString (JSString)

newtype EvStore = EvStore JSVal

foreign import javascript interruptible "require(['ev-store'], $c);"
   requireEvStore :: IO EvStore

foreign import javascript unsafe "if ($1($2)[$3] !== undefined) $1($2)[$3]();"
   dispatchEvent :: EvStore -> DomNode -> JSString -> IO ()
