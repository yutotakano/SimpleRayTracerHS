module RayTracer where

import Vector
import Codec.Picture

data Ray = Ray Vector Vector -- origin, direction
data Colour = RGB Int Int Int deriving Eq

data Object = Box Colour Vector Double Double Double | -- p1, w, h, d
              Plane Colour Vector Double | -- n, d
              Sphere Colour Vector Double -- p1, radius

intersect :: Ray -> Object -> Maybe (Double, Vector, Colour) -- t, normal
-- intersect (Ray origin direction) (Box p1 w h d) =
intersect (Ray origin direction) (Sphere colour p1 r)
  | nabla < 0 = Nothing
  | otherwise = Just (t, normal, colour)
  where
    t = min t1 t2
    t1 = base + sqrt nabla
    t2 = base - sqrt nabla
    base = (-1) * (dotV (unitV direction) (subV origin p1)) 
    nabla = (dotV (unitV direction) (subV origin p1))**2 - ((moduloV (subV origin p1))**2 - r**2)
    normal = unitV $ subV (addV origin (multV (unitV direction) t)) p1

intersect (Ray origin direction) (Plane colour normal d)
  | dotV (unitV direction) normal == 0 = Nothing
  | otherwise                          = Just (t, normal, colour)
  where
    t = ((dotV origin normal) - d) / (dotV normal $ unitV direction) * (-1)

intersect (Ray origin direction) (Box colour (Vector x1 y1 z1) w h d) = findMin trues
  where
    trues = [
      q
      | Just q@(t, normal, colour) <- [
          intersect (Ray origin direction) p | p <- [p1, p2, p3, p4, p5, p6]
        ], 
        let (Vector x y z) = addV origin (multV (unitV direction) t),
        x >= x1, x <= x1 + w,
        y >= y1, y <= y1 + h,
        z >= z1, z <= z1 + d
      ]
    n1 = Vector 0 0 (-1)
    n2 = multV n1 (-1)
    p1 = Plane colour n1 (dotV n1 (Vector x1 y1 z1))
    p2 = Plane colour n2 (dotV n2 (Vector x1 y1 (z1+d)))
    n3 = Vector 0 (-1) 0
    n4 = multV n3 (-1)
    p3 = Plane colour n3 (dotV n3 (Vector x1 y1 z1))
    p4 = Plane colour n4 (dotV n4 (Vector x1 (y1+h) z1))
    n5 = Vector (-1) 0 0
    n6 = multV n5 (-1)
    p5 = Plane colour n5 (dotV n5 (Vector x1 y1 z1))
    p6 = Plane colour n6 (dotV n6 (Vector (x1+w) y1 z1))

findMin :: [(Double, Vector, Colour)] -> Maybe (Double, Vector, Colour) 
findMin [] = Nothing
findMin (a@(t, Vector x y z, m):[]) = Just a
findMin (p@(t, Vector x y z, m):q@(s, Vector a b c, n):xs)
  | t < s = findMin (p:xs)
  | t > s = findMin (q:xs)
  | t == s = findMin (p:xs)

type World = [Object]

data Screen = Screen (Double, Double, Double) Vector -- (w, h, focal), pos

renderAtPixel :: (Screen, World, (Int, Int)) -> Int -> Int -> ((Screen, World, (Int, Int)), PixelRGB8)
renderAtPixel state@((Screen (w, h, focal) pos), objects, (o_w, o_h)) j i = (state, cumulativeToRGB $ getColour)
  where
    getColour :: [Int]
    getColour 
      | iExist    = colourToList $ brighten iColour iBrightness
      | otherwise = [0, 0, 0]
    
    brighten :: Colour -> Double -> Colour
    brighten (RGB r g b) l = RGB adjustedR adjustedG adjustedB
      where
        adjustedR = round ((fromIntegral r)*l/255)
        adjustedG = round ((fromIntegral g)*l/255)
        adjustedB = round ((fromIntegral b)*l/255)
    
    intersection :: Maybe (Double, Vector, Colour)
    intersection = findMin $ [a | Just a <- [intersect (Ray iRayO iRayD) item | item <- objects]]
    --  ++ [(300, Vector 0 0 (-1), RGB 255 255 255)]

    iExist :: Bool
    iExist = intersection /= Nothing

    (Just iDistance, Just iNormal, Just iColour) = distributeMaybe intersection

    iBrightness :: Double    
    iBrightness = (80 * acos (dotV iNormal iRayD / (moduloV iNormal * moduloV iRayD))) / ((iDistance - sDistance) / 20)
      where 
        sDistance :: Double
        sDistance = sqrt (focal**2 + (((fromIntegral i) - (d_h/2))*(h/d_h))**2 + (((fromIntegral j) - (d_w/2))*(h/d_h))**2)
        
    iRayO :: Vector
    iRayO = Vector 0 0 (focal*(-1))

    iRayD :: Vector
    iRayD = subV (Vector (((fromIntegral j) - (d_w/2))*(w/d_w)) (((fromIntegral i) - (d_h/2))*(h/d_h)) 0) iRayO

    d_w = fromIntegral o_w
    d_h = fromIntegral o_h

cumulativeToRGB :: [Int] -> PixelRGB8
cumulativeToRGB (r:g:b:[]) = PixelRGB8 r1 g1 b1
  where
    r1 = fromIntegral $ min 0xff r
    g1 = fromIntegral $ min 0xff g
    b1 = fromIntegral $ min 0xff b

cumulative :: [[Int]] -> [Int]
cumulative [] = []
cumulative (x:[]) = x
cumulative ([r,g,b]:[r1,g1,b1]:[]) = [r+r1, g+g1, b+b1]
cumulative (x:y:xs) = cumulative $ (cumulative [x,y]) : xs

distributeMaybe :: Maybe (Double, Vector, Colour) -> (Maybe Double, Maybe Vector, Maybe Colour)
distributeMaybe Nothing = (Nothing, Nothing, Nothing)
distributeMaybe (Just (a,b,c)) = (Just a, Just b, Just c) 

colourToList :: Colour -> [Int]
colourToList (RGB r g b) = [r,g,b]