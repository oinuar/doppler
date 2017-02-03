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
import Control.Monad
import Data.JSString          (pack)

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
   startEventCapturing

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

startEventCapturing :: IO ()
startEventCapturing = do
   evStore <- requireEvStore

   _ <- forkIO $ forever $ do
      let eventName = pack "click"

      event <- onCapturedEvent eventName
      target <- getEventTarget event
      dispatchEvent evStore target eventName

   return ()

liftEventHandler :: EventHandlerIO s -> EventHandler [Action] s
liftEventHandler handler =
   pure . Action . handler
