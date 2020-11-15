module Main where

import Codec.Picture
import Vector
import RayTracer

outputSize = (512, 512)

main :: IO ()
main = writePng "/mnt/c/Users/moa17/Desktop/test.png" $ snd $ uncurry (generateFoldImage renderAtPixel (Screen 200 200 100, world, outputSize)) outputSize

yellow = RGB 255 215 64 
white = RGB 255 255 255
green = RGB 24 142 112

world = [
  Box white (Vector 0 (-120) 50) 100 100 100, 
  Box yellow (Vector (-100) (-20) (40)) 50 50 50,
  Sphere green (Vector 20 10 30) 20,
  Sphere white (Vector 20 (-20) 30) 15
  ]