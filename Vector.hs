module Vector where

data Vector = Vector Double Double Double deriving (Eq, Show)

(>+<) :: Vector -> Vector -> Vector
(Vector x y z) >+< (Vector a b c) = Vector (x + a) (y + b) (z + c)

(>-<) :: Vector -> Vector -> Vector
(Vector x y z) >-< (Vector a b c) = Vector (x - a) (y - b) (z - c)

(>/<) :: Vector -> Double -> Vector
(Vector x y z) >/< c = Vector (x / c) (y / c) (z / c)

(>*<) :: Vector -> Double -> Vector
(Vector x y z) >*< c = Vector (x * c) (y * c) (z * c)

moduloV :: Vector -> Double
moduloV (Vector x y z) = sqrt $ x**2 + y**2 + z**2

unitV :: Vector -> Vector
unitV v = v >/< (moduloV v)

(•)  :: Vector -> Vector -> Double
(Vector x y z) • (Vector a b c) = (x * a) + (y * b) + (z * c)

(×) :: Vector -> Vector -> Vector
(Vector x y z) × (Vector a b c) = Vector (y*c - z*b) (z*a - x*c) (x*b - y*a)