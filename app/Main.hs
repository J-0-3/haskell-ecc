module Main (main) where

import Lib (doublePoint, CurvePoint(..), multiplyPoint)

main :: IO ()
main = do
  let c = (497, 1768, 9739)
  let multiplyPoint' = multiplyPoint c
  let doublePoint' = doublePoint c

  print $ multiplyPoint' (Point 5323 5438) 1337
