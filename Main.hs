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
  marble <- getTexture "marble.jpg"
  -- list of all objects in the world
  let objects = [
        -- ground
        Plane white (Vector 0 1 0) (Vector 0 g 0),
        -- wall
        Box white (Vector (-400) g 152) 600 300 20,
        Box white (Vector (-400) g 150) 600 7 2,

        -- table legs
        Box marble (Vector 0 g 100) 10 80 10,
        Box marble (Vector 140 g 100) 10 80 10,
        Box marble (Vector 0 g 140) 10 80 10,
        Box marble (Vector 140 g 140) 10 80 10,
        -- tabletop
        Box marble (Vector 0 (g+80) 100) 150 4 50,
        -- table drawers
        Box marble (Vector 90 (g) (100)) 50 20 50,
        Box marble (Vector 111 (g+9) 97) 8 2 3,
        Box marble (Vector 90 (g+20) 95) 50 30 50,
        Box marble (Vector 111 (g+34) 92) 8 2 3,
        Box marble (Vector 90 (g+50) 100) 50 30 50,
        Box marble (Vector 111 (g+64) 97) 8 2 3,

        -- desktop
        Box yellow (Vector 15 (g+94) 130) 70 40 2,
        --neck
        Box yellow (Vector 48 (g+84) 132) 4 10 2,
        --base
        Box yellow (Vector 40 (g+84) 126) 20 2 10,
        -- keyboard
        Box yellow (Vector 34 (g+84) 110) 32 (0.5) 9
        ]
  -- illuminations
  let lights = [
        SphericalLight lamp 300 (Vector 200 (g+300) 0) 40,
        SphericalLight lamp 300 (Vector (-200) (g+300) 0) 40]
  
  -- render the images for the provided positions
  -- camera needs to go from 0,0,0 to 50,-6,100
  -- so, interpolate from 0..150 and divide each value accordingly
  let images = [
        renderSingle (Vector x y z) (960, 540) (objects, lights)
        | i <- [0],
          let j = (fromIntegral i),
          let x = j/3,
          let y = j/(-25),
          let z = j/(1.5)]

  -- convert to GIF and output it
  fromRight (return ()) $ writeGifAnimation "output.gif" 5 LoopingForever images

-- | Renders a single frame from the given position of the camera, output resolution, and [objects, lights]
renderSingle :: Vector -> (Int, Int) -> World -> Image PixelRGB8
renderSingle pos outputSize world = snd $ uncurry (generateFoldImage renderAtPixel (Screen (160, 90, 100) pos, world, outputSize, False)) outputSize

-- | Taken from `fromRight` in the newer versions of Prelude 
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b