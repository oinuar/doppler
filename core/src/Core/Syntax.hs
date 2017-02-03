module Core.Syntax (
   parseWhitespaces, parseJunk, parseLineEnds, parseInterpolationContent,
   lexeme, skipLineEnds
) where

import Text.Parsec
import Language.Haskell.TH
import Text.Parsec.String  (Parser)

parseWhitespaces :: Parser String
parseWhitespaces =
   many $ space <|> tab

parseJunk :: Parser String
parseJunk =
   many $ space <|> tab <|> endOfLine

parseLineEnds :: Parser String
parseLineEnds =
   many endOfLine

parseInterpolationContent :: Parser String
parseInterpolationContent =
   between (char '{') (char '}') characters
   where
      characters =
         many1 $ alphaNum <|> char ' '

lexeme :: Parser a -> Parser a
lexeme p =
   p <* parseWhitespaces

skipLineEnds :: Parser a -> Parser a
skipLineEnds p =
   p <* parseLineEnds
