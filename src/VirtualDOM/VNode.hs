{-# LANGUAGE JavaScriptFFI #-}

module VirtualDOM.VNode (
   VNode, VTree (getRoot), createDomNode, linkVTree, unlinkVTree
) where

import HTML.Types
import VirtualDOM.VDom
import Event
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback
import JavaScript.Object
import qualified CSS.Types  as CSS
import DOM                  (DomNode)
import Data.Char            (toLower)
import Data.JSString        (pack)
import Control.Applicative  ((<$>))
import JavaScript.Array     (JSArray, fromList)

newtype VNode = VNode JSVal

instance IsJSVal VNode

data VTree = VTree {
   getRoot :: VNode,
   getAttachedEventListeners :: [Callback (JSVal -> IO ())]
}


linkVTree :: VDom -> Expression -> IO VTree
linkVTree vdom expression = do
   (root, eventListeners) <- toVNode' expression
   return VTree {
      getRoot = root,
      getAttachedEventListeners = eventListeners
   }
   where
      toVNode' :: Expression -> IO (VNode, [Callback (JSVal -> IO ())])
      toVNode' (Text content) =
         flip (,) [] <$> mkTextNode vdom (pack content)

      toVNode' (Element tagName attributes children) = do
         (attrs, eventListeners) <- foldr setAttribute (flip (,) [] <$> create) attributes
         childs <- mapM toVNode' children
         node <- mkNode vdom (pack tagName) attrs (fromList $ map (jsval . fst) childs)
         return (node, eventListeners ++ concatMap snd childs)

      toVNode' _ =
         flip (,) [] <$> mkTextNode vdom (pack "")

      setAttribute (Attribute (key, Values values)) acc = do
         (obj, eventListeners) <- acc

         case toStringVal key values `failback` toCssVal key values `failback` toCallbackVal key values of
            -- Set property with a custom key (because of wrapped event listeners) and
            -- extend the list of outstanding event listeners.
            (k, Just v) -> do
               (v', eventListeners') <- v
               setProp k v' obj
               return (obj, eventListeners' ++ eventListeners)

            -- Set property to null ref if it has no values.
            (k, Nothing) -> do
               setProp k nullRef obj
               return (obj, eventListeners)

      setAttribute _ acc =
         acc

      failback _ rhs@(_, Just _) =
         rhs

      failback lhs _ =
         lhs

      toStringVal :: Key -> [Value] -> (JSString, Maybe (IO (JSVal, [Callback (JSVal -> IO ())])))
      toStringVal key =
         (,) (pack key) . fmap toStringVal' . foldr joinString Nothing
         where
            toStringVal' =
               return . flip (,) [] . jsval . pack

      toCssVal :: Key -> [Value] -> (JSString, Maybe (IO (JSVal, [Callback (JSVal -> IO ())])))
      toCssVal key =
         (,) (pack key) . fmap toCssVal' . foldr joinCss Nothing
         where
            toCssVal' obj = do
               obj' <- obj
               return (jsval obj', [])

      toCallbackVal :: Key -> [Value] -> (JSString, Maybe (IO (JSVal, [Callback (JSVal -> IO ())])))
      toCallbackVal key =
         case key of
            'o':'n':x:xs -> (,) (mkKey x xs) . fmap toCallbackVal' . foldl joinAction Nothing
            _ -> (,) (pack key) . const Nothing
         where
            toCallbackVal' action = do
               callback <- asyncCallback1 $ runAction action
               return (jsval callback, [callback])

            runAction action val = do
               event <- fromJSVal val

               case event of
                  -- Run a callback if event marshalling succeeded.
                  Just event' -> action $ toEvent event'

                  -- Skip silently if event marshalling failed.
                  Nothing -> return ()

            -- vdom uses hyperscript and it has a hard coded ev-store support that
            -- binds callbacks with ev- prefix. Here we convert event keys to
            -- ev- prefix.
            mkKey x xs =
               pack $ "ev-" ++ toLower x : xs

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

      joinAction :: Maybe (Event -> IO ()) -> Value -> Maybe (Event -> IO ())
      joinAction Nothing (EventValue (Action action)) =
         Just action

      joinAction (Just acc) (EventValue (Action action)) =
         Just $ \event -> do
            acc event
            action event

      joinAction acc _ =
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
