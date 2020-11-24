module RayTracer where

import Vector
import Codec.Picture
import Debug.Trace
import Data.List (transpose)

data Ray = Ray Vector Vector -- origin, direction
data Colour = RGB Int Int Int deriving Eq

defaultTexture :: Image PixelRGB8
defaultTexture = generateImage defaultGen 2 2
  where
    defaultGen :: Int -> Int -> PixelRGB8
    defaultGen i j
      | (i + j) `mod` 2 == 0 = listToRGB [255, 64, 129]
      | otherwise   = listToRGB [177, 237, 139]

mkTexture :: String -> IO (Image PixelRGB8)
mkTexture path = do
  result <- readImage path
  case result of
    Left err -> do
      putStr ("Could not load texture" ++ err)
      return (defaultTexture)
    Right img -> do
      putStr "Loaded texture."
      return (convertRGB8 img)

mkRay :: Vector -> Vector -> Ray
mkRay p v = Ray p (unitV v)

data Object = Box Colour Vector Double Double Double | -- p1, w, h, d
              Plane Colour Vector Vector | -- n, p
              Sphere Colour Vector Double -- p1, radius
              deriving Eq

data Light = SphericalLight Colour Double Vector Double -- intensity, p1, radius

type Intersection = (Double, Vector, Object) -- t, normal, object

-- | Allowed margin of error in intersection, due to floating point precision
allowedMargin :: Double
allowedMargin = 10**(-9)

-- | Main function for intersecting Rays with Objects
intersect :: Ray -> Object -> Maybe Intersection
intersect (Ray origin direction) s@(Sphere colour p1 r)
  | nabla < 0 = Nothing
  | otherwise = Just (t, normal, s)
  where
    t = min t1 t2
    t1 = base + sqrt nabla
    t2 = base - sqrt nabla
    base = (-1) * (direction • (origin >-< p1)) 
    nabla = (direction • (origin >-< p1))**2 - ((moduloV (origin >-< p1))**2 - r**2)
    normal = unitV $ (origin >+< (direction >*< t)) >-< p1

intersect (Ray origin direction) p@(Plane colour normal point)
  | (direction) • normal == 0  = Nothing
  | (point >-< origin) • normal == 0 = Nothing
  | otherwise                        = Just (t, normal, p)
  where
    t = ((point >-< origin) • normal) / (normal • direction)

intersect r@(Ray origin direction) (Box colour (Vector x1 y1 z1) w h d) = findClosest trues
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
    n2 = Vector 0 0 1
    p2 = Plane colour n2 (Vector x1 y1 (z1+d))
    n3 = Vector 0 (-1) 0
    p3 = Plane colour n3 (Vector x1 y1 z1)
    n4 = Vector 0 1 0
    p4 = Plane colour n4 (Vector x1 (y1+h) z1)
    n5 = Vector (-1) 0 0
    p5 = Plane colour n5 (Vector x1 y1 z1)
    n6 = Vector 1 0 0
    p6 = Plane colour n6 (Vector (x1+w) y1 z1)

-- | Find closest intersection based on distance
-- Gets on the positive result.
findClosest :: [Intersection] -> Maybe Intersection
findClosest [] = Nothing
findClosest (a@(t, Vector x y z, m):[]) 
  | t >= 0 = Just a
  | t < 0 = Nothing
findClosest (p@(t, Vector x y z, m):q@(s, Vector a b c, n):xs)
  | t >= 0 && t < s = findClosest (p:xs)
  | s >= 0 && s < t = findClosest (q:xs)
  | t >= 0 && t == s = findClosest (p:xs)
  | otherwise = findClosest (q:xs)

-- | Calculate the light on a point as an RGB list of Doubles
-- Divide RGB by 255, multiply by (255/(pi/2)), multiply by angle, and divide by distance^2, scaled by intensity
lightContribution :: Vector -> Vector -> Object -> Light -> [Double]
lightContribution c n o (SphericalLight colour intensity point radius)
  | n • v < -allowedMargin = [0, 0, 0]
  | otherwise              = map (\x -> x / 255 * 162 * (1.57 - angleBetween) / (moduloV v / intensity)**2) $ baseColour
    where
      angleBetween = acos (n • v / (moduloV n * moduloV v))
      baseColour = map fromIntegral $ colourToList colour
      v = point >-< c


type World = ([Object], [Light])

data Screen = Screen (Double, Double, Double) Vector -- (w, h, focal), pos

type Resolution = (Int, Int)

-- | Called by JuicyPixels for each pixel to render
-- Scale the pixel coordinate to the according screen coordinate, 
-- create a ray and shoot it from the origin (0, 0, focal point)
-- get all intersections to the ray, and the colour of the intersected object
-- and convert to RGB
renderAtPixel :: (Screen, World, Resolution) -> Int -> Int -> ((Screen, World, Resolution), PixelRGB8)
renderAtPixel s@((Screen (w, h, focal) pos), (objects, lights), (o_w, o_h)) x' y' = (s, listToRGB $ getColour)
  where
    x = fromIntegral x'
    y = fromIntegral y'

    getColour :: [Int]
    getColour 
      | iExist    = iColour
      | otherwise = [0, 0, 0]
    
    intersection :: Maybe Intersection
    intersection = findClosest $ [a | Just a <- [intersect (mkRay (pos >+< iRayO) iRayD) item | item <- objects]]
    --  ++ [(300, Vector 0 0 (-1), RGB 255 255 255)]

    (Just iDistance, Just iNormal, Just iObject) = distributeMaybe intersection
    iCoord = pos >+< iRayO >+< ((unitV iRayD) >*< iDistance)

    iExist :: Bool
    iExist = intersection /= Nothing && if iDistance > 0 then True else traceShow (iDistance) False

    iColour :: [Int]
    iColour = map (round . sum) $ transpose [lightContribution iCoord iNormal iObject light | light <- lights]

    iRayO :: Vector
    iRayO = Vector 0 0 (focal*(-1))

    iRayD :: Vector
    iRayD = (Vector (x*w/d_w - w/2) (h/2 - y*h/d_h) 0) >-< iRayO

    d_w = fromIntegral o_w
    d_h = fromIntegral o_h

-- | Convert list of three Ints to RGB
listToRGB :: [Int] -> PixelRGB8
listToRGB (r:g:b:[]) = PixelRGB8 r1 g1 b1
  where
    r1 = fromIntegral $ min 0xff r
    g1 = fromIntegral $ min 0xff g
    b1 = fromIntegral $ min 0xff b

-- | Distribute Maybe tuple into a tuple of Maybes
distributeMaybe :: Maybe Intersection -> (Maybe Double, Maybe Vector, Maybe Object)
distributeMaybe Nothing = (Nothing, Nothing, Nothing)
distributeMaybe (Just (a,b,c)) = (Just a, Just b, Just c) 

-- | Convert custom Color tuple to list of three Ints
colourToList :: Colour -> [Int]
colourToList (RGB r g b) = [r,g,b]