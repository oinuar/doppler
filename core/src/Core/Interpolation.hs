module Core.Interpolation (
   runInterpolation
) where

import Language.Haskell.TH

runInterpolation :: String -> Q Exp
runInterpolation =
   foldl1 appE . map (varE . mkName) . words
