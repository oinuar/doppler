{-# LANGUAGE QuasiQuotes #-}

module Entry where

import Doppler.Html.Types
import Doppler.Html.Syntax

newtype Entry = Entry String deriving Eq
newtype Entries = Entries [Entry] deriving Eq

instance IsHtml Entries where
   formatHtml (Entries entries) =
      [html|<ul>${entries}</ul>|]

instance IsHtml Entry where
   formatHtml (Entry content) =
      [html|<li>${content}</li>|]

addEntry :: Entries -> Entry -> Entries
addEntry (Entries entries) =
   Entries . flip (:) entries
