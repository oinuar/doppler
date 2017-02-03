{-# LANGUAGE JavaScriptFFI #-}

module EvStore (
   EvStore, requireEvStore, delegateEvent
) where

import GHCJS.Types   (JSVal)
import Data.JSString (JSString)

newtype EvStore = EvStore JSVal

foreign import javascript interruptible "require(['ev-store'], $c);"
   requireEvStore :: IO EvStore

-- TODO: delegate event parameters also.
foreign import javascript unsafe "if ($3.target !== undefined && $1($3.target)[$2] !== undefined) $1($3.target)[$2]();"
   delegateEvent :: EvStore -> JSString -> JSVal -> IO ()
