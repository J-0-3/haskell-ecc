module Lib (doublePoint, addPoints, CurvePoint(Point, Infinity), modInverse) where

type WeierstrassCurve = (Integer, Integer, Integer)

data CurvePoint = Infinity | Point Integer Integer deriving (Show)

modInverse' :: Integer -> Integer -> Integer -> Integer -> Integer
modInverse' _ t2 1 _ = t2
modInverse' t1 t2 x m = modInverse' t2 (t1 - (m `div` x) * t2) (m `mod` x) x

modInverse :: Integer -> Integer -> Integer
modInverse x m = mod (modInverse' 0 1 x m) m

tangentGradAtPoint :: WeierstrassCurve -> CurvePoint -> Integer
tangentGradAtPoint (a, _, m) (Point x y) = ((3 * (x ^ 2) + a) * modInverse (2 * y) m) `mod` m
tangentGradAtPoint _ Infinity = error "Cannot find tangent at infinity"

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
    λ = ((y' - y) * modInverse (x' - x) m) `mod` m
    x'' = (λ ^ 2 - x - x') `mod` m
addPoints _ (Point x y) Infinity = Point x y
addPoints _ Infinity (Point x y) = Point x y
addPoints _ Infinity Infinity = Infinity
