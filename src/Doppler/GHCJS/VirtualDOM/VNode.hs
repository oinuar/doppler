{-# LANGUAGE JavaScriptFFI #-}

module Doppler.GHCJS.VirtualDOM.VNode (
   VNode, VTree (getRoot), createDomNode, linkVTree, unlinkVTree
) where

import Doppler.Html.Types
import Doppler.GHCJS.VirtualDOM.VDom
--import Doppler.GHCJS.Event
import GHCJS.Types
--import GHCJS.Marshal
import GHCJS.Foreign.Callback
import JavaScript.Object
import qualified Doppler.Css.Types   as Css
import Doppler.GHCJS.DOM             (DomNode)
import Data.JSString                 (pack)
import Control.Applicative           ((<$>))
import JavaScript.Array              (JSArray, fromList)

newtype VNode = VNode JSVal

instance IsJSVal VNode

data VTree = VTree {
   getRoot :: VNode,
   getAttachedEventListeners :: [Callback (JSVal -> IO ())]
}


linkVTree :: VDom -> Html -> IO VTree
linkVTree vdom doc = do
   (root, eventListeners) <- toVNode' $ getHtml doc
   return VTree {
      getRoot = root,
      getAttachedEventListeners = eventListeners
   }
   where
      toVNode' :: Tag HtmlAttribute HtmlContent -> IO (VNode, [Callback (JSVal -> IO ())])
      toVNode' (FullTag tagName attributes children) = do
         (attrs, eventListeners) <- foldr setAttribute (flip (,) [] <$> create) attributes
         childs <- mapM toVNode' children
         node <- mkNode vdom (pack tagName) attrs (fromList $ map (jsval . fst) childs)
         return (node, eventListeners ++ concatMap snd childs)

      toVNode' (ShortTag tagName attributes) =
         toShortOrDanglingVNode tagName attributes

      toVNode' (DanglingTag tagName attributes) =
         toShortOrDanglingVNode tagName attributes

      toVNode' (Content (Plain content)) =
         flip (,) [] <$> mkTextNode vdom (pack content)

      toVNode' (Content BreakingSpace) =
         flip (,) [] <$> mkTextNode vdom (pack " ")

      toVNode' _ =
         flip (,) [] <$> mkTextNode vdom (pack "")

      toShortOrDanglingVNode tagName attributes = do
         (attrs, eventListeners) <- foldr setAttribute (flip (,) [] <$> create) attributes
         node <- mkNode vdom (pack tagName) attrs (fromList [])
         return (node, eventListeners)

      setAttribute (key, []) acc = do
         (obj, eventListeners) <- acc
         setProp (pack key) nullRef obj
         return (obj, eventListeners)

      setAttribute (key, values) acc = do
         (obj, eventListeners) <- acc
         styleObj <- create
         styleObj' <- buildStyleObject styleObj values

         val <- case styleObj' of
            Just styleObj'' -> return styleObj''
            Nothing -> let (Value value) = mconcat values
                       in return . jsval . pack $ value

         setProp (pack key) val obj
         return (obj, eventListeners)

      buildStyleObject :: Object -> [HtmlAttributeValue] -> IO (Maybe JSVal)
      buildStyleObject obj (StyleValue (Css.CssProperty (name, values)) : xs) = do
         let (Css.Value value) = mconcat values
         setProp (pack name) (jsval . pack $ value) obj

         case xs of
            [] -> return . Just . jsval $ obj
            _ -> buildStyleObject obj xs

      buildStyleObject _ _ =
         return Nothing

unlinkVTree :: VTree -> IO VTree
unlinkVTree tree = do
   mapM_ releaseCallback $ getAttachedEventListeners tree
   return tree { getAttachedEventListeners = [] }

foreign import javascript unsafe "$1.create($2)"
   createDomNode :: VDom -> VNode -> IO DomNode

foreign import javascript unsafe "new $1.VText($2)"
   mkTextNode :: VDom -> JSString -> IO VNode

foreign import javascript unsafe "$1.h($2, $3, $4)"
   mkNode :: VDom -> JSString -> Object -> JSArray -> IO VNode
