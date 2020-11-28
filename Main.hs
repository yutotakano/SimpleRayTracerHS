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
      bluelight = PixelRGB8 236 243 255
      sun = PixelRGB8 246 209 0
  -- material can be both colour and texture (loaded with getTexture)
  let white = Colour (PixelRGB8 255 255 255)
      orange = Colour (PixelRGB8 238 162 57)
      silver = Colour (PixelRGB8 197 197 197)
      darksilver = Colour (PixelRGB8 124 124 124)
      blue = Colour (PixelRGB8 136 175 238)
  edinburgh <- getTexture "textures/edinburgh.jpg"
  wood <- getTexture "textures/wood.jpg"
  face <- getTexture "textures/face.png"
  zoom <- getTexture "textures/zoom.png"
  marble <- getTexture "textures/marble.jpg"
  -- list of all objects in the world
  let objects = [
        -- ground
        Box wood (Vector (-300) (g-10) 0) 600 10 262,
        -- ceiling
        Box white (Vector (-300) (g+300) 0) 600 10 262,

        -- wall
        Box blue (Vector (-300) g 252) 600 116 10,
        Box blue (Vector (-300) (g+270) 252) 600 30 10,
        Box blue (Vector (-300) (g+116) 252) 98 154 10,
        Box blue (Vector (2) (g+116) 252) 298 154 10,
        Box white (Vector (-300) g 250) 600 7 2,
        -- window
        Box white (Vector (-202) (g+116) 246) 204 6 16,
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
        Box silver (Vector 131 (g+9) 197) 8 2 3,
        Box marble (Vector 110 (g+20) 195) 50 30 50,
        Box silver (Vector 131 (g+34) 192) 8 2 3,
        Box marble (Vector 110 (g+50) 200) 50 30 50,
        Box silver (Vector 131 (g+64) 197) 8 2 3,

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
        -- mouse
        Ellipsoid darksilver (Vector 100 (g+84) 220) 3 2 4,

        Cylinder wood (Vector 120 (g+84) 220) 10 10,

        -- bookshelf
        Box wood (Vector 170 g 220) 100 3 30,
        Box wood (Vector 170 (g+297) 220) 100 3 30,
        Box wood (Vector 170 (g+3) 220) 3 294 30,
        Box wood (Vector 267 (g+3) 220) 3 294 30,

        -- books
        Box wood (Vector 173 (g+33) 220) 94 3 30,
        Box wood (Vector 173 (g+66) 220) 94 3 30,
        Box wood (Vector 173 (g+100) 220) 94 3 30,
        Box wood (Vector 173 (g+150) 220) 94 3 30,
        Box wood (Vector 173 (g+180) 220) 94 3 30,
        Box wood (Vector 173 (g+210) 220) 94 3 30,
        Box wood (Vector 173 (g+250) 220) 94 3 30,
        Box wood (Vector 173 (g+280) 220) 94 3 30,

        -- face
        Ellipsoid face (Vector (-5) (g+120) 40) 7.5 12 10,
        -- body
        Ellipsoid marble (Vector (-5) (g+70) 40) 15 50 10
        ]
  -- illuminations
  let lights = [
        -- SphericalLight sun 500 (Vector 0 500 0) 0,  
        -- uncomment when Shadow is On
        SphericalLight lamp 80 (Vector 200 (g+290) 100) 40,
        SphericalLight lamp 80 (Vector (-200) (g+290) 100) 40,
        SphericalLight bluelight 10 (Vector 0 0 (-60)) 30
        ]
  
  -- render the images for the provided positions
  -- camera needs to go from 0,0,0 to 60,10,100 (center is 50, -6, 320)
  -- so, interpolate from 0..150 and divide each value accordingly
  let images = [
        renderSingle (Vector x y z) (1920, 1080) (objects, lights)
        | i <- [0, 75, 140, 150],
          let j = (fromIntegral i),
          let x = j/1.634615,
          let y = j/(44), -- so much manual tweaking was done with these three values... ;( time gone
          let z = j/(150/(226.75))]

  -- render each out as a png instead of GIF for two reasons:
  -- 1. I can visibly see the progress, useful for estimation (not possible with lazy eval monads)
  -- 2. I can stop halfway through and still use the data
  -- 3. I can combine into GIF using an external tool anyway
  mapM_ renderImage (zip images [0..])

  -- convert to GIF and output it
  -- fromRight (return ()) $ writeGifAnimation "output.gif" 50 LoopingForever images

-- | Renders an image out to a file
renderImage :: (Image PixelRGB8, Int) -> IO ()
renderImage (image, i) = writePng ("output/" ++ (reverse . take 3 . reverse) ("000" ++ show i) ++ ".png") $ image  

-- | Renders a single frame from the given position of the camera, output resolution, and [objects, lights]
renderSingle :: Vector -> (Int, Int) -> World -> Image PixelRGB8
renderSingle pos outputSize world = snd $ uncurry (generateFoldImage renderAtPixel (Screen (1.7777, 1, 1) pos, world, outputSize, False)) outputSize

-- | Taken from `fromRight` in the newer versions of Prelude 
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b