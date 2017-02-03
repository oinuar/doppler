module View (
   View (..), render
) where

import VirtualDOM.VDom
import VirtualDOM.VNode
import VirtualDOM.Patch
import DOM
import EvStore
import Event
import HTML.Types
import Control.Concurrent
import Control.Concurrent.STM
import GHCJS.Foreign.Callback
import GHCJS.Types            (JSVal)
import Data.JSString          (JSString, pack)

class View s where
   mkView :: EventHandler [Action] s -> s -> Expression

render :: (Eq s, View s) => s -> IO ()
render initialState = do
   stateVar <- newTVarIO initialState

   vdom <- requireVDom
   tree <- linkVTree vdom $ mkView (liftEventHandler $ runEvent stateVar) initialState
   root <- createDomNode vdom $ getRoot tree
   replaceBody root

   _ <- forkIO $ monitorStateChange stateVar initialState vdom tree root
   startAsyncEventCapture

monitorStateChange :: (Eq s, View s) => TVar s -> s -> VDom -> VTree -> DomNode -> IO ()
monitorStateChange stateVar oldState vdom tree root = do
   (newState, expression) <- atomically $ do
      newState <- readTVar stateVar
      check (oldState /= newState)
      return (newState, mkView (liftEventHandler $ runEvent stateVar) newState)

   newTree <- linkVTree vdom expression
   patches <- diff vdom (getRoot tree) (getRoot newTree)
   newRoot <- patch vdom root patches
   _ <- unlinkVTree tree

   monitorStateChange stateVar newState vdom newTree newRoot

startAsyncEventCapture :: IO ()
startAsyncEventCapture = do
   evStore <- requireEvStore

   -- TODO: when to release the callback?
   _ <- captureEvent evStore $ pack "click"

   -- TODO: add more captures.

   return ()

captureEvent :: EvStore -> JSString -> IO (Callback (JSVal -> IO ()))
captureEvent evStore name = do
   callback' <- asyncCallback1 callback
   startEventCapturing name callback'
   return callback'
   where
      callback event = do
         target <- getEventTarget event
         dispatchEvent evStore target name

liftEventHandler :: EventHandlerIO s -> EventHandler [Action] s
liftEventHandler handler =
   pure . Action . handler
