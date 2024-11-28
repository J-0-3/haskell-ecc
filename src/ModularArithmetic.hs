module ModularArithmetic (modInverse, modSquareRoot) where

modInverse' :: Integer -> Integer -> Integer -> Integer -> Integer
modInverse' _ t2 1 _ = t2
modInverse' t1 t2 x m = modInverse' t2 (t1 - (m `div` x) * t2) (m `mod` x) x

modInverse :: Integer -> Integer -> Integer
modInverse 0 _ = error "Cannot compute inverse 0 (undefined)"
modInverse x m
  | gcd x m == 1 = mod (modInverse' 0 1 x m) m
  | otherwise = error "Cannot compute inverse over divisible modulus"

modSquareRoot :: Integer -> Integer -> Integer
modSquareRoot x m
  | m `mod` 4 == 3 = (x ^ ((m + 1) `div` 4)) `mod` m
  | otherwise = error "Unable to compute square root when non-congruent to 3 (mod 4)"
