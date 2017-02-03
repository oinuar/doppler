module Main where

import Application (ApplicationState (..))
import View        (render)

main :: IO ()
main =
   render ApplicationState {
      getNumberOfClicks = 0
   }
