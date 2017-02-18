module Main where

import Application (ApplicationState (..))
import View        (render)
import Entry

main :: IO ()
main =
   render ApplicationState {
      getEntries = Entries [],
      getNewEntryValue = ""
   }
