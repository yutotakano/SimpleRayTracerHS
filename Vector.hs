module Vector where

data Vector = Vector Double Double Double deriving (Eq, Show)

addV :: Vector -> Vector -> Vector
addV (Vector x y z) (Vector a b c) = Vector (x + a) (y + b) (z + c)

subV :: Vector -> Vector -> Vector
subV (Vector x y z) (Vector a b c) = Vector (x - a) (y - b) (z - c)

divV :: Vector -> Double -> Vector
divV (Vector x y z) c = Vector (x / c) (y / c) (z / c)

multV :: Vector -> Double -> Vector
multV (Vector x y z) c = Vector (x * c) (y * c) (z * c)

moduloV :: Vector -> Double
moduloV (Vector x y z) = sqrt $ x**2 + y**2 + z**2

unitV :: Vector -> Vector
unitV v = divV v (moduloV v)

dotV :: Vector -> Vector -> Double
dotV (Vector x y z) (Vector a b c) = (x * a) + (y * b) + (z * c)

(•) = dotV

crossV :: Vector -> Vector -> Vector
crossV (Vector x y z) (Vector a b c) = Vector (y*c - z*b) (z*a - x*c) (x*b - y*a)