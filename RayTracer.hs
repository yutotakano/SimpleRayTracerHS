module RayTracer where

import Vector
import Codec.Picture
import Debug.Trace

data Ray = Ray Vector Vector -- origin, direction
data Colour = RGB Int Int Int deriving Eq

mkRay :: Vector -> Vector -> Ray
mkRay p v = Ray p (unitV v)

data Object = Box Colour Vector Double Double Double | -- p1, w, h, d
              Plane Colour Vector Vector | -- n, p
              Sphere Colour Vector Double -- p1, radius

type Intersection = (Double, Vector, Colour)

allowedMargin :: Double
allowedMargin = 10**(-9)

intersect :: Ray -> Object -> Maybe Intersection -- t, normal
-- intersect (Ray origin direction) (Box p1 w h d) =
intersect (Ray origin direction) (Sphere colour p1 r)
  | nabla < 0 = Nothing
  | otherwise = Just (t, normal, colour)
  where
    t = min t1 t2
    t1 = base + sqrt nabla
    t2 = base - sqrt nabla
    base = (-1) * (direction • (origin >-< p1)) 
    nabla = (direction • (origin >-< p1))**2 - ((moduloV (origin >-< p1))**2 - r**2)
    normal = unitV $ (origin >+< (direction >*< t)) >-< p1

intersect (Ray origin direction) (Plane colour normal point)
  | (direction) • normal == 0  = Nothing
  | (point >-< origin) • normal == 0 = Nothing
  | otherwise                        = Just (t, normal, colour)
  where
    t = ((point >-< origin) • normal) / (normal • direction)

intersect r@(Ray origin direction) (Box colour (Vector x1 y1 z1) w h d) = findMin trues
  where
    trues = [
      q
      | Just q@(t, normal, colour) <- [
          intersect r p | p <- [p1, p2, p3, p4, p5, p6]
        ], 
        let (Vector x y z) = origin >+< (direction >*< t),
        x - x1 >= (-allowedMargin), (-allowedMargin) <= x1 + w - x,
        y - y1 >= (-allowedMargin), (-allowedMargin) <= y1 + h - y,
        z - z1 >= (-allowedMargin), (-allowedMargin) <= z1 + d - z
      ]
    n1 = Vector 0 0 (-1)
    p1 = Plane colour n1 (Vector x1 y1 z1)
    p2 = Plane colour n1 (Vector x1 y1 (z1+d))
    n3 = Vector 0 (-1) 0
    p3 = Plane colour n3 (Vector x1 y1 z1)
    p4 = Plane colour n3 (Vector x1 (y1+h) z1)
    n5 = Vector (-1) 0 0
    p5 = Plane colour n5 (Vector x1 y1 z1)
    p6 = Plane colour n5 (Vector (x1+w) y1 z1)

findMin :: [Intersection] -> Maybe Intersection
findMin [] = Nothing
findMin (a@(t, Vector x y z, m):[]) = Just a
findMin (p@(t, Vector x y z, m):q@(s, Vector a b c, n):xs)
  | t < s = findMin (p:xs)
  | t > s = findMin (q:xs)
  | t == s = findMin (p:xs)

type World = [Object]

data Screen = Screen (Double, Double, Double) Vector -- (w, h, focal), pos

type Resolution = (Int, Int)

renderAtPixel :: (Screen, World, Resolution) -> Int -> Int -> ((Screen, World, Resolution), PixelRGB8)
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
    
    intersection :: Maybe Intersection
    intersection = findMin $ [a | Just a <- [intersect (mkRay (pos >+< iRayO) iRayD) item | item <- objects]]
    --  ++ [(300, Vector 0 0 (-1), RGB 255 255 255)]

    (Just iDistance, Just iNormal, Just iColour) = distributeMaybe intersection

    iExist :: Bool
    iExist = intersection /= Nothing && iDistance > 0

    iBrightness :: Double    
    iBrightness = (81 * acos (iNormal • (unitV iRayD) / (moduloV iNormal))) / (iDistance / 50)**2
      where
        sDistance :: Double
        sDistance = sqrt (focal**2 + ((fromIntegral i)*h/d_h - h/2)**2 + ((fromIntegral j)*w/d_w - w/2)**2)

    iRayO :: Vector
    iRayO = Vector 0 0 (focal*(-1))

    iRayD :: Vector
    iRayD = (Vector ((fromIntegral j)*w/d_w - w/2) ((fromIntegral i)*h/d_h - h/2) 0) >-< iRayO

    d_w = fromIntegral o_w
    d_h = fromIntegral o_h

cumulativeToRGB :: [Int] -> PixelRGB8
cumulativeToRGB (r:g:b:[]) = PixelRGB8 r1 g1 b1
  where
    r1 = fromIntegral $ min 0xff r
    g1 = fromIntegral $ min 0xff g
    b1 = fromIntegral $ min 0xff b

distributeMaybe :: Maybe Intersection -> (Maybe Double, Maybe Vector, Maybe Colour)
distributeMaybe Nothing = (Nothing, Nothing, Nothing)
distributeMaybe (Just (a,b,c)) = (Just a, Just b, Just c) 

colourToList :: Colour -> [Int]
colourToList (RGB r g b) = [r,g,b]