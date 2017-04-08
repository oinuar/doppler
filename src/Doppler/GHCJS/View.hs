module Doppler.GHCJS.View (
   View (..), render
) where

import Doppler.GHCJS.VirtualDOM.VDom
import Doppler.GHCJS.VirtualDOM.VNode
import Doppler.GHCJS.VirtualDOM.Patch
import Doppler.GHCJS.DOM
import Doppler.GHCJS.EvStore
import Doppler.GHCJS.Event
import Doppler.Html.Types
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad          (when)
import Control.Exception
import GHCJS.Foreign.Callback
import GHCJS.Types            (JSVal)
import Data.JSString          (JSString, pack)

class View s where
   mkView :: EventHandler [HtmlAction] s -> s -> Html

render :: (Eq s, View s) => s -> IO ()
render initialState = do
   stateVar <- newTVarIO initialState
   synchronizationVar <- newTVarIO True

   vdom <- requireVDom
   tree <- linkVTree vdom $ mkView (runEvent synchronizationVar stateVar) initialState
   root <- createDomNode vdom $ getRoot tree
   replaceBody root

   _ <- forkIO $ monitorStateChange synchronizationVar stateVar initialState vdom tree root
   startEventSinks

monitorStateChange :: (Eq s, View s) => TVar Bool -> TVar s -> s -> VDom -> VTree -> DomNode -> IO ()
monitorStateChange synchronizationVar stateVar oldState vdom tree root = do
   (newState, doc) <- atomically $ do
      newState <- readTVar stateVar
      check (oldState /= newState)
      return (newState, mkView (runEvent synchronizationVar stateVar) newState)

   newTree <- linkVTree vdom doc
   patches <- diff vdom (getRoot tree) (getRoot newTree)
   newRoot <- patch vdom root patches
   _ <- unlinkVTree tree

   monitorStateChange synchronizationVar stateVar newState vdom newTree newRoot

startEventSinks :: IO ()
startEventSinks = do
   evStore <- requireEvStore

   -- Capturing is never stopped. These are global captures and when the
   -- JS compartment of the browser is destroyed, RTS and JS memory also
   -- gets destroyed (and these listeners will too). Main point is that
   -- we don't create new listeners every time: we only have one listener
   -- for each event type that are alive as long as the page itself is alive.
   mapM_ (startEventSink evStore . pack) ["click",
                                          "change",
                                          "input"] -- TODO: add more captures.

startEventSink :: EvStore -> JSString -> IO (Callback (JSVal -> IO ()))
startEventSink evStore name = do
   delegate <- asyncCallback1 $ delegateEvent evStore name
   startEventCapturing name delegate
   return delegate

runEvent :: TVar Bool -> TVar s -> EventHandler [HtmlAction] s
runEvent synchronizationVar stateVar handler =
   [HtmlAction runStateUpdate]
   where
      runStateUpdate event = do
         state <- atomically readSync
         handle handleException $ runHandler event state

      runHandler event state = do
         state' <- handler event state
         atomically $ writeSync state'

      handleException :: SomeException -> IO ()
      handleException e = do
         atomically finishSync
         throw e

      readSync = do
         state <- readTVar stateVar
         synchronized <- readTVar synchronizationVar
         check synchronized
         writeTVar synchronizationVar False
         return state

      writeSync state = do
         writeTVar stateVar state
         finishSync

      finishSync = do
         synchronized <- swapTVar synchronizationVar True
         when synchronized $ fail "State should not be synchronized at this point, but it was"
