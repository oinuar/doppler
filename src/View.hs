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
import Control.Monad          (when)
import Control.Exception
import GHCJS.Foreign.Callback
import GHCJS.Types            (JSVal)
import Data.JSString          (JSString, pack)

class View s where
   mkView :: EventHandler [Action] s -> s -> Expression

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
   (newState, expression) <- atomically $ do
      newState <- readTVar stateVar
      check (oldState /= newState)
      return (newState, mkView (runEvent synchronizationVar stateVar) newState)

   newTree <- linkVTree vdom expression
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
   _ <- startEventSink evStore $ pack "click"

   -- TODO: add more captures.

   return ()

startEventSink :: EvStore -> JSString -> IO (Callback (JSVal -> IO ()))
startEventSink evStore name = do
   delegate <- asyncCallback1 $ delegateEvent evStore name
   startEventCapturing name delegate
   return delegate

runEvent :: TVar Bool -> TVar s -> EventHandler [Action] s
runEvent synchronizationVar stateVar handler =
   [Action runStateUpdate]
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
