module Main (main) where

import PointArithmetic (doublePoint, CurvePoint(..), multiplyPoint)

main :: IO ()
main = do
  let c = (497, 1768, 9739)

  print $ multiplyPoint' (Point 5323 5438) 1337
