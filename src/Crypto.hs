module Crypto where

import PointArithmetic (CurvePoint (Point), WeierstrassCurve, getYCoordinates, multiplyPoint, xCoord)

dhGenPublicKey :: WeierstrassCurve -> CurvePoint -> Integer -> Integer
dhGenPublicKey c g s = xCoord $ multiplyPoint c g s

dhCalcShared :: WeierstrassCurve -> Integer -> Integer -> Integer
dhCalcShared c p s = xCoord $ multiplyPoint c (Point p $ fst $ getYCoordinates c p) s
