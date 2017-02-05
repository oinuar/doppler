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
   tree <- linkVTree vdom $ mkView (runEvent stateVar) initialState
   root <- createDomNode vdom $ getRoot tree
   replaceBody root

   _ <- forkIO $ monitorStateChange stateVar initialState vdom tree root
   startEventSinks

monitorStateChange :: (Eq s, View s) => TVar s -> s -> VDom -> VTree -> DomNode -> IO ()
monitorStateChange stateVar oldState vdom tree root = do
   (newState, expression) <- atomically $ do
      newState <- readTVar stateVar
      check (oldState /= newState)
      return (newState, mkView (runEvent stateVar) newState)

   newTree <- linkVTree vdom expression
   patches <- diff vdom (getRoot tree) (getRoot newTree)
   newRoot <- patch vdom root patches
   _ <- unlinkVTree tree

   monitorStateChange stateVar newState vdom newTree newRoot

startEventSinks :: IO ()
startEventSinks = do
   evStore <- requireEvStore

   -- Capturing is never stopped. These are global captures and when the
   -- JS compartment of the browser is destroyed, RTS and JS memory also
   -- gets destroyed (and these listeners will too). Main point is that
   -- we don't create new listeners every time: we only have one listener
   -- for each event type that are alive as long as the page itself is alive.
   _ <- startEventSink evStore $ pack "click"

   -- TODO: add more captures.

   return ()

startEventSink :: EvStore -> JSString -> IO (Callback (JSVal -> IO ()))
startEventSink evStore name = do
   delegate <- asyncCallback1 $ delegateEvent evStore name
   startEventCapturing name delegate
   return delegate

runEvent :: TVar s -> EventHandler [Action] s
runEvent stateVar handler =
   [Action runStateUpdate]
   where
      runStateUpdate event =
         atomically $ modifyTVar stateVar $ handler event
