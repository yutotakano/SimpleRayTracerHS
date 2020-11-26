module RayTracer (Texture(..), getTexture, Object(..), Light(..), World, Screen(..), renderAtPixel)
where

import Vector
import Codec.Picture
import Debug.Trace
import Data.List (transpose)

data Ray = Ray Vector Vector -- origin, direction

data Texture = Colour PixelRGB8 |
               Img (Image PixelRGB8) deriving Eq

defaultTexture :: Image PixelRGB8
defaultTexture = generateImage defaultGen 2 2
  where
    defaultGen :: Int -> Int -> PixelRGB8
    defaultGen i j
      | (i + j) `mod` 2 == 0 = listToRGB8 [255, 64, 129]
      | otherwise   = listToRGB8 [177, 237, 139]

getTexture :: String -> IO (Texture)
getTexture path = do
  result <- readImage path
  case result of
    Left err -> do
      putStrLn ("Could not load texture \"" ++ err ++ "\"")
      return (Img defaultTexture)
    Right img -> do
      putStrLn "Loaded texture."
      return (Img $ convertRGB8 img)

mkRay :: Vector -> Vector -> Ray
mkRay p v = Ray p (unitV v)

data Object = Box Texture Vector Double Double Double | -- p1, w, h, d
              Plane Texture Vector Vector | -- n, p
              Ellipsoid Texture Vector Double Double Double -- p1, radiusx, radiusy, radiusz
              deriving Eq

data Light = SphericalLight PixelRGB8 Double Vector Double -- intensity, p1, radius

type Intersection = (Double, Vector, Object) -- t, normal, object

-- | Allowed margin of error in intersection, due to floating point precision
allowedMargin :: Double
allowedMargin = 10**(-9)

-- | Main function for intersecting Rays with Objects
intersect :: Ray -> Object -> Maybe Intersection
intersect (Ray origin direction) s@(Ellipsoid texture c rx ry rz)
  | nabla < 0 = Nothing
  | otherwise = Just (t, normal, s)
    where
      -- rearranged from https://cs.oberlin.edu/~bob/cs357.08/VectorGeometry/VectorGeometry.pdf
      timesm :: Vector -> Vector
      timesm (Vector x y z) = Vector (x/rx) (y/ry) (z/rz)
      v1 = (timesm direction)
      p1 = (timesm origin) >-< (timesm c)
      t = min t1 t2
      t1 = (base + sqrt nabla) / (v1 • v1)
      t2 = (base - sqrt nabla) / (v1 • v1)
      base = - (p1 • v1)
      nabla = ((p1 • v1)**2 - (p1 • p1 - 1)*(v1 • v1))
      normal = unitV $ (origin >+< (direction >*< t)) >-< c

intersect (Ray origin direction) p@(Plane texture normal point)
  | (direction) • normal == 0  = Nothing
  | (point >-< origin) • normal == 0 = Nothing
  | otherwise                        = Just (t, normal, p)
  where
    t = ((point >-< origin) • normal) / (normal • direction)

intersect r@(Ray origin direction) b@(Box texture (Vector x1 y1 z1) w h d) = findClosest trues
  where
    trues = [
      (t, normal, b)
      | Just (t, normal, object) <- [
          intersect r p | p <- [p1, p2, p3, p4, p5, p6]
        ], 
        let (Vector x y z) = origin >+< (direction >*< t),
        x - x1 >= (-allowedMargin), (-allowedMargin) <= x1 + w - x,
        y - y1 >= (-allowedMargin), (-allowedMargin) <= y1 + h - y,
        z - z1 >= (-allowedMargin), (-allowedMargin) <= z1 + d - z
      ]
    n1 = Vector 0 0 (-1)
    p1 = Plane texture n1 (Vector x1 y1 z1)
    n2 = Vector 0 0 1
    p2 = Plane texture n2 (Vector x1 y1 (z1+d))
    n3 = Vector 0 (-1) 0
    p3 = Plane texture n3 (Vector x1 y1 z1)
    n4 = Vector 0 1 0
    p4 = Plane texture n4 (Vector x1 (y1+h) z1)
    n5 = Vector (-1) 0 0
    p5 = Plane texture n5 (Vector x1 y1 z1)
    n6 = Vector 1 0 0
    p6 = Plane texture n6 (Vector (x1+w) y1 z1)

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


-- | Calculate the light on a point as an list of doubles
-- Divide RGB by 255, multiply by (255/(pi/2)), multiply by angle, and divide by distance^2, scaled by intensity
lightContribution :: Vector -> Vector -> Object -> Light -> [Double]
lightContribution c n o (SphericalLight colour intensity point radius)
  | n • v < -allowedMargin = [0, 0, 0]
  | otherwise              = map (\x -> x / 255 * 162 * (1.57 - angleBetween) / (moduloV v / intensity)**2) baseColour
    where
      angleBetween = acos (n • v / (moduloV n * moduloV v))
      baseColour = map (fromIntegral) $ rgb8ToList $ getColourOfObjectAt c o
      v = point >-< c

-- | Throw rays towards light source, find shadows as 0-255 where 255 is darkest
-- The reason it's inverted is because it is going to be subtracted from lightContribution
shadowContribution :: Vector -> Vector -> Object -> Light -> [Object] -> [Double]
shadowContribution c n o (SphericalLight colour intensity point radius) objects = getShadow
  where
    getShadow :: [Double]
    getShadow 
      | (point >-< c) • n < 0 = [0,0,0]
      | intersection = [10, 10, 10] 
      | otherwise = [0, 0, 0]
    
    intersection :: Bool
    intersection = 0 < length [a | Just a <- [intersect (mkRay c (point >-< c)) item | item <- objects, item /= o]]


-- | box labelling for the looong function below
--    +--------------+          +--------------+
--   /      E       /|         /|              |
--  /              / |        / |              |
-- *--------------*  |       *  |      𐐒       |
-- |              |  |       | Ɔ|              |
-- |              | D|       |  |              |
-- |      A       |  |       |  +--------------+
-- |              | /        | /     Ⅎ        /
-- |              |/         |/              /
-- *--------------*          *--------------*

getColourOfObjectAt :: Vector -> Object -> PixelRGB8
getColourOfObjectAt (Vector ix iy iz) (Box texture (Vector x y z) w h d)
  | abs (ix - x) < allowedMargin = 
    -- lies on C
    traceShow "C"
    getColourFromTextureAt ((z + d - iz) / d) ((y + h - iy) / h) texture
  | abs (ix - x - w) < allowedMargin =
    -- lies on D
    traceShow "D"
    getColourFromTextureAt ((iz - z) / d) ((y + h - iy) / h) texture
  | abs (iy - y) < allowedMargin =
    -- lies on F
    traceShow "F"
    getColourFromTextureAt ((x + w - ix) / w) ((iz - z) / d) texture
  | abs (iy - y - h) < allowedMargin =
    -- lies on E
    traceShow "E"
    getColourFromTextureAt ((ix - x) / w) ((z + d - iz) / d) texture
  | abs (iz - z) < allowedMargin =
    -- lies on A
    traceShow "A"
    getColourFromTextureAt ((ix - x) / w) ((y + h - iy) / h) texture
  | abs (iz - z - d) < allowedMargin =
    -- lies on B
    traceShow "B"
    getColourFromTextureAt ((x - ix) / w) ((y + h - iy) / h) texture

getColourOfObjectAt (Vector ix iy iz) (Ellipsoid texture p1 rx ry rz) = getColourFromTextureAt 1 1 texture

getColourOfObjectAt (Vector ix iy iz) (Plane texture n p1) = getColourFromTextureAt 1 1 texture

getColourFromTextureAt :: Double -> Double -> Texture -> PixelRGB8
getColourFromTextureAt _ _ (Colour c) = c
getColourFromTextureAt x y (Img im) = pixelAt im (coordx) (coordy)
  where
    coordx = (round . (*) x . fromIntegral) (w - 1)
    coordy = (round . (*) y . fromIntegral) (h - 1)
    w = imageWidth im
    h = imageHeight im
  


type World = ([Object], [Light])

data Screen = Screen (Double, Double, Double) Vector -- (w, h, focal), pos

type Resolution = (Int, Int)

type Shadow = Bool

-- | Called by JuicyPixels for each pixel to render
-- Scale the pixel coordinate to the according screen coordinate, 
-- create a ray and shoot it from the origin (0, 0, focal point)
-- get all intersections to the ray, and the colour of the intersected object
-- and convert to RGB
renderAtPixel :: (Screen, World, Resolution, Shadow) -> Int -> Int -> ((Screen, World, Resolution, Shadow), PixelRGB8)
renderAtPixel s@((Screen (w, h, focal) pos), (objects, lights), (o_w, o_h), shadow) x' y' = (s, listToRGB8 $ getColour)
  where
    x = fromIntegral x'
    y = fromIntegral y'

    getColour :: [Int]
    getColour 
      | iExist    = iColour
      | otherwise = [0, 0, 0]
    
    intersection :: Maybe Intersection
    intersection = findClosest $ [a | Just a <- [intersect (mkRay (pos >+< iRayO) iRayD) item | item <- objects]]

    (Just iDistance, Just iNormal, Just iObject) = distributeMaybe intersection
    iCoord = pos >+< iRayO >+< ((unitV iRayD) >*< iDistance)

    iExist :: Bool
    iExist = intersection /= Nothing && if iDistance > 0 then True else traceShow (iDistance) False

    iColour :: [Int]
    iColour = map (round . sum) $ transpose [
      if not shadow then (lightContribution iCoord iNormal iObject light) else map (max 0) $ zipWith (-) (lightContribution iCoord iNormal iObject light) (shadowContribution iCoord iNormal iObject light objects)
      | light <- lights
      ]

    iRayO :: Vector
    iRayO = Vector 0 0 (focal*(-1))

    iRayD :: Vector
    iRayD = (Vector (x*w/d_w - w/2) (h/2 - y*h/d_h) 0) >-< iRayO

    d_w = fromIntegral o_w
    d_h = fromIntegral o_h

-- | Convert PixelRGB8 to list ot three ints
rgb8ToList :: PixelRGB8 -> [Int]
rgb8ToList (PixelRGB8 r g b) = map (round . fromIntegral) [r, g, b]

-- | Convert list of three Ints to RGB
listToRGB8 :: [Int] -> PixelRGB8
listToRGB8 (r:g:b:[]) = PixelRGB8 r1 g1 b1
  where
    r1 = fromIntegral $ min 0xff r
    g1 = fromIntegral $ min 0xff g
    b1 = fromIntegral $ min 0xff b

-- | Distribute Maybe tuple into a tuple of Maybes
distributeMaybe :: Maybe Intersection -> (Maybe Double, Maybe Vector, Maybe Object)
distributeMaybe Nothing = (Nothing, Nothing, Nothing)
distributeMaybe (Just (a,b,c)) = (Just a, Just b, Just c) 

-- | Convert custom Color tuple to list of three Ints
colourToList :: Texture -> [Int]
colourToList (Colour (PixelRGB8 r g b)) = map (round . fromIntegral) [r,g,b]