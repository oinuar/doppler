{-# LANGUAGE JavaScriptFFI #-}

module VirtualDOM.Patch (
   diff, patch
) where

import VirtualDOM.VDom
import VirtualDOM.VNode
import DOM               (DomNode)
import GHCJS.Types       (JSVal)

newtype Patch = Patch JSVal

foreign import javascript unsafe "$1.diff($2, $3)"
   diff :: VDom -> VNode -> VNode -> IO Patch

foreign import javascript unsafe "$1.patch($2, $3)"
   patch :: VDom -> DomNode -> Patch -> IO DomNode
