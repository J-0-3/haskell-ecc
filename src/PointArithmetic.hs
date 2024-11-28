module PointArithmetic (doublePoint, addPoints, CurvePoint (Point, Infinity), multiplyPoint, getYCoordinates, WeierstrassCurve, xCoord, yCoord) where

import ModularArithmetic (modInverse, modSquareRoot)

type WeierstrassCurve = (Integer, Integer, Integer)

data CurvePoint = Infinity | Point Integer Integer deriving (Show)

xCoord :: CurvePoint -> Integer
xCoord (Point x y) = x
xCoord Infinity = error "Point at Infinity does not have an X coordinate"

yCoord :: CurvePoint -> Integer
yCoord (Point x y) = y
yCoord Infinity = error "Point at Infinity does not have a Y coordinate"

tangentGradAtPoint :: WeierstrassCurve -> CurvePoint -> Integer
tangentGradAtPoint (a, _, m) (Point x y) = ((3 * (x ^ 2) + a) * modInverse (2 * y) m) `mod` m
tangentGradAtPoint _ Infinity = error "Cannot find tangent at infinity"

getYCoordinates :: WeierstrassCurve -> Integer -> (Integer, Integer)
getYCoordinates (a, b, m) x = (root, (-root) `mod` m)
  where
    root = modSquareRoot ((x ^ 3) + a * x + b) m

doublePoint :: WeierstrassCurve -> CurvePoint -> CurvePoint
doublePoint (a, b, m) (Point x y) = Point x' ((λ * x - λ * x' - y) `mod` m)
  where
    λ = tangentGradAtPoint (a, b, m) (Point x y)
    x' = (λ ^ 2 - 2 * x) `mod` m
doublePoint _ Infinity = Infinity

addPoints :: WeierstrassCurve -> CurvePoint -> CurvePoint -> CurvePoint
addPoints (a, b, m) (Point x y) (Point x' y')
  | (x, y) == (x', y') = doublePoint (a, b, m) (Point x y)
  | (x, y) == (x', -y') = Infinity
  | otherwise = Point x'' ((λ * x - λ * x'' - y) `mod` m)
  where
    λ = (((y' - y) `mod` m) * modInverse ((x' - x) `mod` m) m) `mod` m
    x'' = (λ ^ 2 - x - x') `mod` m
addPoints _ (Point x y) Infinity = Point x y
addPoints _ Infinity (Point x y) = Point x y
addPoints _ Infinity Infinity = Infinity

multiplyPoint' :: CurvePoint -> WeierstrassCurve -> CurvePoint -> Integer -> CurvePoint
multiplyPoint' r _ _ 0 = r
multiplyPoint' r c q n = multiplyPoint' (if (n `mod` 2) == 1 then addPoints c r q else r) c (doublePoint c q) (n `div` 2)

multiplyPoint :: WeierstrassCurve -> CurvePoint -> Integer -> CurvePoint
multiplyPoint = multiplyPoint' Infinity
