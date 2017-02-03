{-# LANGUAGE JavaScriptFFI #-}

module VirtualDOM.VNode (
   VNode, VTree (getRoot), createDomNode, linkVTree, unlinkVTree
) where

import HTML.Types
import VirtualDOM.VDom
import GHCJS.Types
import GHCJS.Foreign.Callback
import JavaScript.Object
import qualified CSS.Types  as CSS
import DOM                  (DomNode)
import Data.Char            (toLower)
import Data.JSString        (pack)
import Control.Applicative  ((<$>), (<|>))
import JavaScript.Array     (JSArray, fromList)

newtype VNode = VNode { jsVNode :: JSVal }

data VTree = VTree {
   getRoot :: VNode,
   getAttachedEventListeners :: [Callback (IO ())]
}

linkVTree :: VDom -> Expression -> IO VTree
linkVTree vdom expression = do
   (root, eventListeners) <- toVNode' expression
   return VTree {
      getRoot = root,
      getAttachedEventListeners = eventListeners
   }
   where
      toVNode' :: Expression -> IO (VNode, [Callback (IO ())])
      toVNode' (Text content) =
         flip (,) [] <$> mkTextNode vdom (pack content)

      toVNode' (Element tagName attributes children) = do
         (attrs, eventListeners) <- foldr setAttribute (flip (,) [] <$> create) attributes
         childs <- mapM toVNode' children
         node <- mkNode vdom (pack tagName) attrs (fromList $ map (jsVNode . fst) childs)
         return (node, eventListeners ++ concatMap snd childs)

      toVNode' _ =
         flip (,) [] <$> mkTextNode vdom (pack "")

      setAttribute (Attribute (key, Values values)) acc = do
         (obj, eventListeners) <- acc

         case toStringVal key values <|> toCssVal key values <|> toCallbackVal key values of
            Just (k, v) -> do
               (v', eventListeners') <- v
               setProp k v' obj
               return (obj, eventListeners' ++ eventListeners)
            _ -> return (obj, eventListeners)

      setAttribute _ acc =
         acc

      toStringVal :: Key -> [Value] -> Maybe (JSString, IO (JSVal, [Callback (IO ())]))
      toStringVal key =
         fmap toStringVal' . foldr joinString Nothing
         where
            toStringVal' =
               (,) (pack key) . return . flip (,) [] . jsval . pack

      toCssVal :: Key -> [Value] -> Maybe (JSString, IO (JSVal, [Callback (IO ())]))
      toCssVal key =
         fmap toCssVal' . foldr joinCss Nothing
         where
            toCssVal' obj = (pack key, do
               obj' <- obj
               return (jsval obj', []))

      toCallbackVal :: Key -> [Value] -> Maybe (JSString, IO (JSVal, [Callback (IO ())]))
      toCallbackVal key =
         case key of
            'o':'n':x:xs -> fmap (toCallbackVal' $ mkKey x xs) . foldr joinAction Nothing
            _ -> const Nothing
         where
            toCallbackVal' key' action = (pack key', do
               callback <- asyncCallback action
               return (jsval callback, [callback]))

            mkKey x xs =
               "ev-" ++ toLower x : xs

      joinString :: Value -> Maybe String -> Maybe String
      joinString (StringValue content) Nothing =
         Just content

      joinString (StringValue content) (Just acc) =
         Just $ content ++ acc

      joinString _ acc =
         acc

      joinCssString (CSS.StringValue content) acc =
         content ++ acc

      joinCssString _ acc =
         acc

      joinCss :: Value -> Maybe (IO Object) -> Maybe (IO Object)
      joinCss value@(CSSValue _) Nothing =
         joinCss value $ Just create

      joinCss (CSSValue (CSS.Property (key, value))) (Just obj) = Just $ do
         obj' <- obj
         setProp (pack key) (jsval $ pack $ foldr joinCssString "" value) obj'
         return obj'

      joinCss _ acc =
         acc

      joinAction :: Value -> Maybe (IO ()) -> Maybe (IO ())
      joinAction (EventValue (Action action)) Nothing =
         Just action

      joinAction (EventValue (Action action)) (Just acc) =
         Just $ do { action; acc; }

      joinAction _ acc =
         acc

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
