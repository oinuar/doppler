{-# LANGUAGE QuasiQuotes #-}

module Application (
   ApplicationState (..)
) where

import HTML.Syntax
import CSS.Syntax
import View

newtype ApplicationState = ApplicationState {
   getNumberOfClicks :: Int
} deriving (Eq)

instance View ApplicationState where
   mkView handler currentState =
      [html|
         <div>
            <header>
               <h1>Welcome to my page</h1>
            </header>

            <section style=${stylesheet} className="content">
               <h2>Hello! You have pressed the button ${getNumberOfClicks currentState} times.</h2>
               <button onClick=${handler click}>Press me</button>
            </section>

            <footer>
               <p>All rights reserved, as usual</p>
            </footer>
         </div>
      |]
      where
         stylesheet =
            [style|
               border: 1px solid black;
               padding: 1em;
               background-color: ${selectColor};
            |]

         selectColor =
            case getNumberOfClicks currentState `mod` 3 of
               0 -> "red"
               1 -> "green"
               2 -> "blue"
               _ -> "white"

         click _ state =
            state { getNumberOfClicks = getNumberOfClicks state + 1 }
