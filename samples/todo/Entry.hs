{-# LANGUAGE QuasiQuotes #-}

module Entry where

import Doppler.HTML.Types
import Doppler.HTML.Syntax

newtype Entry = Entry String deriving Eq
newtype Entries = Entries [Entry] deriving Eq

instance IsHTML Entries where
   toExpression (Entries entries) =
      [html|<ul>${entries}</ul>|]

instance IsHTML Entry where
   toExpression (Entry content) =
      [html|<li>${content}</li>|]

addEntry :: Entries -> Entry -> Entries
addEntry (Entries entries) =
   Entries . flip (:) entries
