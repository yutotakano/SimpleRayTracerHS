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
  let bluelight = PixelRGB8 236 243 255
  -- material can be both colour and texture (loaded with getTexture)
  let yellow = Colour (PixelRGB8 255 215 64)
      white = Colour (PixelRGB8 255 255 255)
      orange = Colour (PixelRGB8 238 162 57)
      silver = Colour (PixelRGB8 197 197 197)
      darksilver = Colour (PixelRGB8 124 124 124)
  brick <- getTexture "textures/brick.png"
  edinburgh <- getTexture "textures/edinburgh.jpg"
  face <- getTexture "textures/face.png"
  zoom <- getTexture "textures/zoom.png"
  marble <- getTexture "textures/marble.jpg"
  -- list of all objects in the world
  let objects = [
        -- ground
        Plane white (Vector 0 1 0) (Vector 0 g 0),
        -- wall
        Box orange (Vector (-300) g 252) 600 116 10,
        Box orange (Vector (-300) (g+270) 252) 600 30 10,
        Box orange (Vector (-300) (g+116) 252) 98 154 10,
        Box orange (Vector (2) (g+116) 252) 298 154 10,
        Box white (Vector (-300) g 250) 600 7 2,
        -- window
        Box white (Vector (-202) (g+116) 248) 204 4 16,
        Box white (Vector (-202) (g+120) 250) 2 150 12,
        Box white (Vector (-101) (g+120) 250) 2 150 12,
        Box white (Vector 0 (g+120) 250) 2 150 12,
        Box white (Vector (-202) (g+270) 250) 204 2 12,
        -- outside
        Box edinburgh (Vector (-600) (g+100) 500) 600 338 1,

        -- table legs
        Box marble (Vector 20 g 200) 10 80 10,
        Box marble (Vector 160 g 200) 10 80 10,
        Box marble (Vector 20 g 240) 10 80 10,
        Box marble (Vector 160 g 240) 10 80 10,
        -- tabletop
        Box marble (Vector 20 (g+80) 200) 150 4 50,
        -- table drawers
        Box marble (Vector 110 (g) (200)) 50 20 50,
        Box marble (Vector 131 (g+9) 197) 8 2 3,
        Box marble (Vector 110 (g+20) 195) 50 30 50,
        Box marble (Vector 131 (g+34) 192) 8 2 3,
        Box marble (Vector 110 (g+50) 200) 50 30 50,
        Box marble (Vector 131 (g+64) 197) 8 2 3,

        -- desktop from 35 g+94 130
        -- bezel
        Box silver (Vector 35 (g+94) 229.8) 70 1 2,
        Box silver (Vector 35 (g+94) 229.8) 1 40 2,
        Box silver (Vector 35 (g+94+39) 229.8) 70 1 2,
        Box silver (Vector (35+69) (g+94) 229.8) 1 40 2,
        -- screen
        Box zoom (Vector 36 (g+95) 230) 69 39 1,
        -- neck
        Box darksilver (Vector 68 (g+84) 232) 4 10 2,
        -- base
        Box darksilver (Vector 60 (g+84) 226) 20 2 10,
        -- keyboard
        Box silver (Vector 54 (g+84) 210) 32 (0.5) 9,

        -- face
        Ellipsoid face (Vector (-5) (g+120) 40) 7.5 12 10,
        -- body
        Ellipsoid marble (Vector (-5) (g+70) 40) 15 50 10
        ]
  -- illuminations
  let lights = [
        SphericalLight lamp 80 (Vector 200 (g+300) 200) 40,
        SphericalLight lamp 100 (Vector (-200) (g+300) 200) 40,
        SphericalLight lamp 100 (Vector 200 (g+300) 0) 40,
        SphericalLight lamp 80 (Vector (-200) (g+300) 0) 40,
        SphericalLight lamp 10 (Vector 0 0 (-60)) 30]
  
  -- render the images for the provided positions
  -- camera needs to go from 0,0,0 to 60,10,100 (center is 50, -6, 320)
  -- so, interpolate from 0..150 and divide each value accordingly
  let images = [
        renderSingle (Vector x y z) (192, 108) (objects, lights)
        | i <- [0,10..150],
          let j = (fromIntegral i),
          let x = j/2.09,
          let y = j/(44), -- so much manual tweaking was done with these three values... ;( time gone
          let z = j/(0.656)]

  -- convert to GIF and output it
  fromRight (return ()) $ writeGifAnimation "output.gif" 50 LoopingForever images

-- | Renders a single frame from the given position of the camera, output resolution, and [objects, lights]
renderSingle :: Vector -> (Int, Int) -> World -> Image PixelRGB8
renderSingle pos outputSize world = snd $ uncurry (generateFoldImage renderAtPixel (Screen (1.7777, 1, 1) pos, world, outputSize, False)) outputSize

-- | Taken from `fromRight` in the newer versions of Prelude 
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b