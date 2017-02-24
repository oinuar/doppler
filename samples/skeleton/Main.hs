module Main where

import Application        (ApplicationState (..))
import Doppler.GHCJS.View (render)

main :: IO ()
main =
   render ApplicationState {
      getNumberOfClicks = 0
   }
