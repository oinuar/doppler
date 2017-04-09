{-# LANGUAGE JavaScriptFFI #-}

module Doppler.GHCJS.VirtualDOM.VNode (
   VNode, VTree (getRoot), createDomNode, linkVTree, unlinkVTree
) where

import Doppler.Html.Types
import Doppler.GHCJS.VirtualDOM.VDom
import Doppler.GHCJS.Event
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback
import JavaScript.Object
import qualified Doppler.Css.Types   as Css
import Doppler.GHCJS.DOM             (DomNode)
import Data.Char                     (toLower)
import Data.JSString                 (pack)
import Control.Applicative           ((<$>), liftA2, (<|>))
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

      setAttribute (key, AttributeValues []) acc = do
         (obj, eventListeners) <- acc
         setProp (pack key) nullRef obj
         return (obj, eventListeners)

      setAttribute (key, AttributeValues values) acc = do
         (obj, eventListeners) <- acc
         result <-               buildStyleObject Nothing key values
                   `alternative` buildCallback key values
                   `alternative` buildValue key values

         case result of
            Just (key', val, listeners) -> do
               setProp key' val obj
               return (obj, eventListeners ++ listeners)

            Nothing -> do
               let (Value value) = mconcat values
               setProp (pack key) (jsval $ pack value) obj
               return (obj, eventListeners)

      setAttribute _ acc =
         acc

      alternative =
         liftA2 (<|>)

      buildValue :: HtmlAttributeName -> [HtmlAttributeValue] -> IO (Maybe (JSString, JSVal, [Callback (JSVal -> IO ())]))
      buildValue key values =
         case joinValues values of
            Just value -> return $ Just (pack key, jsval $ pack value, [])
            Nothing -> return Nothing
         where
            joinValues (a@Value{} : b@Value{} : xs) =
               let x = a `mappend` b
               in joinValues (x : xs)

            joinValues [Value x] =
               Just x

            joinValues _ =
               Nothing

      buildStyleObject :: Maybe Object -> HtmlAttributeName -> [HtmlAttributeValue] -> IO (Maybe (JSString, JSVal, [Callback (JSVal -> IO ())]))
      buildStyleObject obj key (StyleValue (Css.CssProperty (name, values)) : xs) =
         case mconcat values of
            Css.Value value -> do
               obj' <- maybe create return obj
               setProp (pack name) (jsval $ pack value) obj'
               buildStyleObject (Just obj') key xs
            _ -> return Nothing

      buildStyleObject (Just obj) key [] =
         return $ Just (pack key, jsval obj, [])

      buildStyleObject _ _ _ =
         return Nothing

      buildCallback :: HtmlAttributeName -> [HtmlAttributeValue] -> IO (Maybe (JSString, JSVal, [Callback (JSVal -> IO ())]))
      buildCallback ('o' : 'n' : firstLetter : restLetters) values =
         case joinEventValues values of
            Just action -> do
               callback <- asyncCallback1 $ execute action
               return $ Just (mkKey firstLetter restLetters, jsval callback, [callback])
            Nothing -> return Nothing
         where
            joinEventValues (a@EventValue{} : b@EventValue{} : xs) =
               let x = a `mappend` b
               in joinEventValues (x : xs)

            joinEventValues [EventValue x] =
               Just x

            joinEventValues _ =
               Nothing

            execute action val = do
               event <- fromJSVal val

               case event of
                  -- Run a callback if event marshalling succeeded.
                  Just event' -> runAction action $ toEvent event'

                  -- Skip silently if event marshalling failed.
                  Nothing -> return ()

            -- vdom uses hyperscript and it has a hard coded ev-store support that
            -- binds callbacks with ev- prefix. Here we convert event keys to
            -- ev- prefix.
            mkKey x xs =
               pack $ "ev-" ++ toLower x : xs

      buildCallback _ _ =
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
