{-# LANGUAGE QuasiQuotes #-}

module Application (
   ApplicationState (..)
) where

import Doppler.Html.Syntax
import Doppler.GHCJS.View
import Doppler.Event.Types
import Entry

data ApplicationState = ApplicationState {
   getEntries :: Entries,
   getNewEntryValue :: String
} deriving (Eq)

instance View ApplicationState where
   mkView handler currentState =
      [html|
         <div>
            <header>
               <h1>TODO</h1>
            </header>

            <section>
               ${getEntries currentState}
            </section>

            <footer>
               <input type="text" onInput=${handler newEntryValueInput} value="${getNewEntryValue currentState}" />
               <button type="button" onClick=${handler addNewClick} disabled=${getDisabled}>Add new thing</button>
            </footer>
         </div>
      |]
      where
         getDisabled
            | getNewEntryValue currentState == "" = ["disabled"]
            | otherwise = []

         newEntryValueInput event state = do
            print event
            return state { getNewEntryValue = getValue event }

         addNewClick _ state =
            let entry = Entry $ getNewEntryValue state
            in return state { getEntries = addEntry (getEntries state) entry,
                              getNewEntryValue = "" }
