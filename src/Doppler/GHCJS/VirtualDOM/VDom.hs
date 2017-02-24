{-# LANGUAGE JavaScriptFFI #-}

module Doppler.GHCJS.VirtualDOM.VDom (
   VDom, requireVDom
) where

import GHCJS.Types (JSVal)

newtype VDom = VDom JSVal

foreign import javascript interruptible "require(['virtual-dom'], $c);"
   requireVDom :: IO VDom
