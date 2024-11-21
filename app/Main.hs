module Main (main) where

import Lib (doublePoint, CurvePoint(..))

main :: IO ()
main = do
  let c = (497, 1768, 9739)
  let doublePoint' = doublePoint c

  print $ doublePoint' $ Point 5274 2841
