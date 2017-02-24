module Main where

import Entry
import Application        (ApplicationState (..))
import Doppler.GHCJS.View (render)

main :: IO ()
main =
   render ApplicationState {
      getEntries = Entries [],
      getNewEntryValue = ""
   }
