module Main where

import Codec.Picture
import Vector
import Debug.Trace
import RayTracer

-- default ground y-level
g = -120

main :: IO ()
main = do
  -- lights can only have a colour
  let lamp = PixelRGB8 231 227 216
  -- material can be both colour and texture (loaded with getTexture)
  let yellow = Colour (PixelRGB8 255 215 64)
      white = Colour (PixelRGB8 255 255 255)
      green = Colour (PixelRGB8 24 142 112)
  wood <- getTexture "wall.png"
  -- list of all objects in the world
  let objects = [
        -- ground
        Plane white (Vector 0 1 0) (Vector 0 g 0),
        -- wall
        Box wood (Vector (-400) g 150) 600 300 20,

        -- table legs
        Box yellow (Vector 0 g 100) 10 80 10,
        Box yellow (Vector 140 g 100) 10 80 10,
        Box yellow (Vector 0 g 140) 10 80 10,
        Box yellow (Vector 140 g 140) 10 80 10,
        -- tabletop
        Box white (Vector 0 (g+80) 100) 150 4 50,
        -- table drawers
        Box white (Vector 80 (g) (100)) 60 20 50,
        Box white (Vector 80 (g+20) 95) 60 30 50,
        Box white (Vector 80 (g+50) 100) 60 30 50
        ]
  -- illuminations
  let lights = [
        SphericalLight lamp 300 (Vector 200 (g+300) 0) 40,
        SphericalLight lamp 300 (Vector (-200) (g+300) 0) 40]
  
  -- render the images for the provided positions
  let images = [renderSingle p (1920, 1080) (objects, lights) | p <- [0..5]]

  -- convert to GIF and output it
  fromRight (return ()) $ writeGifAnimation "output.gif" 5 LoopingForever images

-- | Renders a single frame from the given position of the camera, output resolution, and [objects, lights]
renderSingle :: Double -> (Int, Int) -> World -> Image PixelRGB8
renderSingle p outputSize world = snd $ uncurry (generateFoldImage renderAtPixel (Screen (160, 90, 100) (Vector 0 0 p), world, outputSize)) outputSize

-- | Taken from `fromRight` in the newer versions of Prelude 
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b