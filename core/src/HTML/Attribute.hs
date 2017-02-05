{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module HTML.Attribute (
   Attribute (..), Key, Value (..), Collection (..), IsHTMLAttribute (..),
   Action (..)
) where

import Core.Interpolation
import CSS.Types                  (Property)
import Event.Types
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type Key = String
newtype Action = Action (Event -> IO ())

data Value =
     StringValue String
   | InterpolationValue String
   | CSSValue Property
   | EventValue Action

data Collection =
     Values [Value]
   | Interpolation String
   deriving (Eq, Show)

newtype Attribute = Attribute {
   getAttribute :: (Key, Collection)
} deriving (Eq, Show)

class IsHTMLAttribute a where
   toAttributeValue :: a -> Value

instance IsHTMLAttribute Value where
   toAttributeValue = id

instance IsHTMLAttribute Bool where
   toAttributeValue = StringValue . show

instance IsHTMLAttribute Char where
   toAttributeValue = StringValue . pure

instance IsHTMLAttribute Double where
   toAttributeValue = StringValue . show

instance IsHTMLAttribute Float where
   toAttributeValue = StringValue . show

instance IsHTMLAttribute Int where
   toAttributeValue = StringValue . show

instance IsHTMLAttribute Property where
   toAttributeValue = CSSValue

instance IsHTMLAttribute Action where
   toAttributeValue = EventValue

instance Lift Value where
   lift (StringValue content) =
      [|StringValue content|]

   lift (InterpolationValue content) =
      appE [|StringValue|] $ runInterpolation content

   lift (CSSValue props) =
      [|CSSValue props|]

   lift (EventValue _) =
      error "Internal error, event values cannot be lifted"

instance Lift Collection where
   lift (Values values) =
      [|Values values|]

   lift (Interpolation content) =
      appE [|Values . map toAttributeValue|] $ runInterpolation content

instance Lift Attribute where
   lift (Attribute attr) =
      [|Attribute attr|]

instance Eq Value where
   (==) (StringValue lhs) (StringValue rhs) =
      lhs == rhs

   (==) (InterpolationValue lhs) (InterpolationValue rhs) =
      lhs == rhs

   (==) (CSSValue lhs) (CSSValue rhs) =
      lhs == rhs

   (==) (EventValue _) (EventValue _) =
      True

   (==) _ _ =
      False

instance Show Value where
   show (StringValue value) =
      "StringValue " ++ value

   show (InterpolationValue value) =
      "InterpolationValue " ++ value

   show (CSSValue value) =
      "CSSValue " ++ show value

   show (EventValue _) =
      "EventValue"
