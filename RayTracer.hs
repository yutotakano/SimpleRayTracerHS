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
      putStrLn ("Loaded texture: " ++ path)
      return (Img $ convertRGB8 img)

mkRay :: Vector -> Vector -> Ray
mkRay p v = Ray p (unitV v)

data Object = Box Texture Vector Double Double Double | -- p1, w, h, d
              Plane Texture Vector Vector | -- n, p
              Ellipsoid Texture Vector Double Double Double | -- p1, radiusx, radiusy, radiusz
              Cylinder Texture Vector Double Double | -- p1, radius height
              CylinderBody Texture Vector Double Double
              deriving Eq

data Light = SphericalLight PixelRGB8 Double Vector Double -- intensity, p1, radius
             

type Intersection = (Double, Vector, Object) -- t, normal, object

-- | Allowed margin of error in intersection, due to floating point precision
allowedMargin :: Double
allowedMargin = 10**(-9)

-- | Main function for intersecting Rays with Objects
intersect :: Ray -> Object -> Maybe Intersection
-- Intersect with Ellipsoid.
-- Adapted using math from https://cs.oberlin.edu/~bob/cs357.08/VectorGeometry/VectorGeometry.pdf 
intersect (Ray origin direction) s@(Ellipsoid texture c rx ry rz)
  | nabla < 0 = Nothing
  | otherwise = Just (t, normal, s)
    where
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

-- Intersect with Plane. Just solve equation.
intersect (Ray origin direction) p@(Plane texture normal point)
  | (direction) • normal == 0  = Nothing
  | (point >-< origin) • normal == 0 = Nothing
  | otherwise                        = Just (t, normal, p)
  where
    t = ((point >-< origin) • normal) / (normal • direction)

-- Intersect with Box. 
-- Intersect with all 6 planes, check if within box dimensions, get closest.
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

-- Intersect with Cylinder.
-- Adapted from https://www.cl.cam.ac.uk/teaching/1999/AGraphHCI/SMAG/node2.html
-- And https://mrl.cs.nyu.edu/~dzorin/cg05/lecture12.pdf
-- Added custom math to rotate Cylinder by any normal
intersect r@(Ray origin direction) cyl@(Cylinder texture p1@(Vector x y z) rad h)
  | maincyl /= Nothing = maincyl
  | otherwise = findClosest caps
  where
    maincyl = intersect r (CylinderBody texture p1 rad h)

    caps = [
      (t, normal, cyl)
      | Just (t, normal, object) <- [
          intersect r c | c <- [cap1, cap2]
        ], 
        let vec = origin >+< (direction >*< t),
        moduloV (vec >-< p1) - rad <= allowedMargin || moduloV (vec >-< (Vector x (y+h) z)) - rad <= allowedMargin
      ]
    
    cap1 = Plane texture (Vector 0 (-1) 0) p1
    cap2 = Plane texture (Vector 0 1 0) (Vector x (y+h) z)

intersect r@(Ray o d) cyl@(CylinderBody texture p1@(Vector x y z) radius height)
  | nabla <= 0 = Nothing
  | t1 < 0 && t2 < 0 = Nothing
  | i_y - y < -allowedMargin || y + height - i_y < -allowedMargin = Nothing
  | otherwise = Just (t, normal, cyl)
  where
    (Vector o_x o_y o_z) = o
    (Vector d_x d_y d_z) = d
    
    a = d_x**2 + d_z**2
    b = 2*(o_x*d_x + o_z*d_z - x*d_x - z*d_z)
    c = o_x**2 + o_z**2 - 2*x*o_x - 2*z*o_z + x**2 + z**2 - radius**2 - 1
    
    nabla = b**2 - 4*a*c
    t1 = (-b + sqrt nabla) / (2*a)
    t2 = (-b - sqrt nabla) / (2*a)
    t | t1 < 0 && t2 >= 0 = t2
      | t1 >= 0 && t1 < 0 = t1
      | otherwise = min t1 t2
    i@(Vector i_x i_y i_z) = o >+< (d >*< t)
    normal = unitV $ i >-< (Vector x y i_z)


-- | Find closest intersection based on distance
-- Gets only the positive result.
findClosest :: [Intersection] -> Maybe Intersection
findClosest [] = Nothing
findClosest (a@(t, Vector x y z, m):[]) 
  | t >= 0 = Just a
  | t < 0 = Nothing
findClosest (p@(t, Vector x y z, m):q@(s, Vector a b c, n):xs)
  | t < 0 && s < 0  = findClosest xs
  | t < 0 && s >= 0 = findClosest (q:xs)
  | t >= 0 && s < 0 = findClosest (p:xs)
  | t < s = findClosest (p:xs)
  | s < t = findClosest (q:xs)
  | t == s = findClosest (p:xs)

-- | Calculate the light on a point as an list of doubles
-- Divide RGB by 255, multiply by (255/(pi/2)), multiply by angle, and divide by distance^2, scaled by intensity
lightContribution :: Vector -> Vector -> Object -> Light -> [Double]
lightContribution c n o (SphericalLight colour intensity point radius)
  | n • v < -allowedMargin = [0, 0, 0]
  | otherwise              = map (\x -> (n • v) / (moduloV n * moduloV v) * x * intensity / (4 * pi * (v • v / 10000))) baseColour
    where
      baseColour = map (fromIntegral) $ rgb8ToList $ getColourOfObjectAt c o
      v = point >-< c

-- | Throw rays towards light source, find shadows as 0-255 where 255 is darkest
-- The reason it's inverted is because it is going to be subtracted from lightContribution
shadowContribution :: Vector -> Vector -> Object -> Light -> [Object] -> [Double]
shadowContribution c n o (SphericalLight colour intensity point radius) objects = getShadow
  where
    getShadow :: [Double]
    getShadow 
      | v • n < 0 = [0,0,0]
      | doesIntersect = map (\x -> (n • v) / (moduloV n * moduloV v) * x * intensity/ (4 * pi * (v • v / 10000))) [255, 255, 255] 
      | otherwise = [0, 0, 0]
      where
    
    v :: Vector
    v = point >-< c
    
    doesIntersect :: Bool
    doesIntersect = checkObjects (mkRay c v) objects
    
    checkObjects :: Ray -> [Object] -> Bool
    checkObjects r [] = False
    checkObjects r (x:xs)
      | x == o = checkObjects r xs
      | a == Nothing = checkObjects r xs
      | t > (moduloV v) = checkObjects r xs
      | t <= 0 = checkObjects r xs
      | otherwise = True
        where
          a = intersect r x
          (Just t, Just normal, Just p) = distributeMaybe a


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
-- UV Mapping for box
getColourOfObjectAt (Vector ix iy iz) (Box texture (Vector x y z) w h d)
  | abs (ix - x) < allowedMargin = 
    -- lies on C
    getColourFromTextureAt ((z + d - iz) / d) ((y + h - iy) / h) texture
  | abs (ix - x - w) < allowedMargin =
    -- lies on D
    getColourFromTextureAt ((iz - z) / d) ((y + h - iy) / h) texture
  | abs (iy - y) < allowedMargin =
    -- lies on F
    getColourFromTextureAt ((x + w - ix) / w) ((iz - z) / d) texture
  | abs (iy - y - h) < allowedMargin =
    -- lies on E
    getColourFromTextureAt ((ix - x) / w) ((z + d - iz) / d) texture
  | abs (iz - z) < allowedMargin =
    -- lies on A
    getColourFromTextureAt ((ix - x) / w) ((y + h - iy) / h) texture
  | abs (iz - z - d) < allowedMargin =
    -- lies on B
    getColourFromTextureAt ((x - ix) / w) ((y + h - iy) / h) texture
-- UV Mapping for ellipsoid. Requires a 3.14x1 image
-- Adapted and modified from:
-- https://gamedev.stackexchange.com/questions/114412/how-to-get-uv-coordinates-for-sphere-cylindrical-projection/114416
getColourOfObjectAt vec@(Vector ix iy iz) (Ellipsoid texture p1 rx ry rz) = getColourFromTextureAt u v texture
  where
    (Vector p1_x p1_y p1_z) = p1
    (Vector nx ny nz) = unitV $ vec >-< p1
    u = ((atan2 nx (-nz) / (2*3.15) + 0.5) - 0.04) `mod'` 1
    v = ((p1_y + ry - iy) / (2*ry)) `mod'` 1
-- Main body cylinder, similar to Sphere
getColourOfObjectAt vec@(Vector ix iy iz) (CylinderBody texture (Vector px py pz) r h) = getColourFromTextureAt u v texture
  where
    (Vector nx ny nz) = unitV (vec >-< (Vector px py iz))
    u = ((atan2 nx (-nz) / (2*3.15) + 0.5) - 0.04) `mod'` 1
    v = (py+h-iy)/h

-- Cap part of cylinder
getColourOfObjectAt vec@(Vector ix iy iz) (Cylinder texture (Vector px py pz) r h)
  | abs (iy - py) < allowedMargin = 
    getColourFromTextureAt (((r + ix - px)/(2*r)) `mod'` 1) (((3*r - iz - pz)/(2*r)) `mod'` 1) texture
  | otherwise = 
    getColourFromTextureAt (((3*r - ix - px)/(2*r)) `mod'` 1) (((3*r - iz - pz)/(2*r)) `mod'` 1) texture

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
renderAtPixel :: (Screen, World, Resolution, Shadow) -> Int -> Int -> PixelRGB8
renderAtPixel s@((Screen (w, h, focal) pos), (objects, lights), (o_w, o_h), shadow) x' y' = listToRGB8 $ getColour
  where
    x = fromIntegral x'
    y = fromIntegral y'

    getColour :: [Int]
    getColour 
      | iExist    = iColour
      | otherwise = [0, 0, 0]
    
    intersection :: Maybe Intersection
    intersection = findClosest [a | Just a <- [intersect (mkRay (pos >+< iRayO) iRayD) item | item <- objects]]

    (Just iDistance, Just iNormal, Just iObject) = distributeMaybe intersection
    iCoord = pos >+< iRayO >+< ((unitV iRayD) >*< iDistance)

    iExist :: Bool
    iExist = intersection /= Nothing 

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

-- | Generalisation of 'div' to any instance of 'Real'
-- Taken from Data.Fixed
div' :: (Real a,Integral b) => a -> a -> b
div' n d = floor ((toRational n) / (toRational d))

-- | Generalisation of 'mod' to any instance of 'Real'
-- Taken from Data.Fixed
mod' :: (Real a) => a -> a -> a
mod' n d = n - (fromInteger f) * d where
    f = div' n d