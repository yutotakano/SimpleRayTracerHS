module RayTracer where

import Vector
import Codec.Picture

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
        let (Vector x y z) = addV origin (multV (unitV direction) t),
        x >= x1, x <= x1 + w,
        y >= y1, y <= y1 + h,
        z >= z1, z <= z1 + d
      ]
    n1 = Vector 0 0 (-1)
    n2 = multV n1 (-1)
    p1 = Plane n1 (dotV n1 (Vector x1 y1 z1))
    p2 = Plane n2 (dotV n2 (addV (Vector x1 y1 z1) (Vector 0 0 d)))
    n3 = Vector 0 (-1) 0
    n4 = multV n3 (-1)
    p3 = Plane n3 (dotV n3 (Vector x1 y1 z1))
    p4 = Plane n4 (dotV n4 (addV (Vector x1 y1 z1) (Vector 0 h 0)))
    n5 = Vector (-1) 0 0
    n6 = multV n5 (-1)
    p5 = Plane n5 (dotV n5 (Vector x1 y1 z1))
    p6 = Plane n6 (dotV n6 (addV (Vector x1 y1 z1) (Vector w 0 0)))

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
    let jitter = [(-1.0)/4.0, 3.0/4.0
                  , 3.0/4.0,  1.0/3.0
                  , (-3.0)/4.0, (-1.0)/4.0
                  , 1.0/4.0, (-3.0)/4.0
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
    let t_toscreen = sqrt (focal**2 + (((fromIntegral i) - (d_h/2))*(h/d_h))**2 + (((fromIntegral j) - (d_w/2))*(h/d_h))**2),
    let brightness = (80 * acos (dotV normal ray_d / (moduloV normal * moduloV ray_d))) / ((t - t_toscreen) / 20)
  ] / 4))
