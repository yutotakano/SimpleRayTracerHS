module Vector where

import Codec.Picture

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

crossV :: Vector -> Vector -> Vector
crossV (Vector x y z) (Vector a b c) = Vector (y*c - z*b) (z*a - x*c) (x*b - y*a)

data Ray = Ray Vector Vector -- origin, direction

data Object = Box Vector Double Double Double | -- p1, w, h, d
              Plane Vector Double | -- n, d
              Sphere Vector Double -- p1, radius

intersect :: Ray -> Object -> Maybe (Double, Vector) -- t, normal
-- intersect (Ray origin direction) (Box p1 w h d) =
intersect (Ray origin direction) (Sphere p1 r)
  | nabla < 0 = Nothing
  | otherwise = Just (t, normal)
  where
    t = min t1 t2
    t1 = base + sqrt nabla
    t2 = base - sqrt nabla
    base = (-1) * (dotV (unitV direction) (subV origin p1)) 
    nabla = (dotV (unitV direction) (subV origin p1))**2 - ((moduloV (subV origin p1))**2 - r**2)
    normal = unitV $ subV (addV origin (multV (unitV direction) t)) p1

intersect (Ray origin direction) (Plane normal d)
  | dotV (unitV direction) normal == 0 = Nothing
  | otherwise                          = Just (t, normal)
  where
    t = ((dotV origin normal) - d) / (dotV normal $ unitV direction) * (-1)

intersect (Ray origin direction) (Box (Vector x1 y1 z1) w h d) = findMin trues
  where
    trues = [
      q
      | Just q@(t, normal) <- [
          intersect (Ray origin direction) p | p <- [p1, p2, p3, p4, p5, p6]
        ], 
        let (Vector x y z) = addV origin (multV direction t),
        x >= x1, x <= x1 + w,
        y >= y1, y <= y1 + h,
        z >= z1, z <= z1 + d
      ]
    n1 = Vector 0 0 1
    p1 = Plane n (dotV n (Vector x1 y1 z1))
      where
        n = multV n1 (-1)
    p2 = Plane n1 (dotV n1 (addV (Vector x1 y1 z1) (Vector 0 0 d)))
    n2 = Vector 0 1 0
    p3 = Plane n (dotV n (Vector x1 y1 z1))
      where
        n = multV n2 (-1)
    p4 = Plane n2 (dotV n2 (addV (Vector x1 y1 z1) (Vector 0 h 0)))
    n3 = Vector 1 0 0
    p5 = Plane n (dotV n (Vector x1 y1 z1))
      where
        n = multV n3 (-1)
    p6 = Plane n3 (dotV n3 (addV (Vector x1 y1 z1) (Vector w 0 0)))

findMin :: [(Double, Vector)] -> Maybe (Double, Vector) 
findMin [] = Nothing
findMin (a@(t, Vector x y z):[]) = Just a
findMin (p@(t, Vector x y z):q@(s, Vector a b c):xs)
  | t < s = findMin (p:xs)
  | t > s = findMin (q:xs)
  | t == s = findMin (p:xs)

type World = [Object]

data Screen = Screen Double Double Double -- w, h, focal

renderAtPixel :: (Screen, World, (Int, Int)) -> Int -> Int -> ((Screen, World, (Int, Int)), Pixel8)
renderAtPixel state@((Screen w h focal), objects, (o_w, o_h)) j i = (state, fromIntegral $ min 0xff $ round (sum [
    if exists then brightness else 0 | sample <- [0..4],
    let jitter = [
      (-1.0)/4.0,  3.0/4.0,
       3.0/4.0,  1.0/3.0,
      (-3.0)/4.0, (-1.0)/4.0,
       1.0/4.0, (-3.0)/4.0
      ],
    let d_w = fromIntegral o_w,
    let d_h = fromIntegral o_h,
    let ray_o = Vector 0 0 (focal*(-1)),
    let ray_d = subV (Vector (((fromIntegral j) + (head $ drop (2*sample - 2) jitter) - (d_w/2))*(w/d_w)) (((fromIntegral i) + (head $ drop (2*sample - 1) jitter) - (d_h/2))*(h/d_h)) 0) ray_o,
    let ray = Ray ray_o ray_d,
    let intersection = findMin $ [a | Just a <- [intersect ray item | item <- objects]] ++ [(300, Vector 0 0 (-1))],
    let exists = intersection /= Nothing,
    let Just t = fmap fst $ intersection,
    let Just normal = fmap snd $ intersection,
    let brightness = (80 * acos (dotV normal ray_d / (moduloV normal * moduloV ray_d))) / (t / 50)
  ] / 4))
