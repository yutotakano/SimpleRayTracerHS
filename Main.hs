module Main where

import Codec.Picture
import Vector
import RayTracer

outputSize = (512, 512)

main :: IO ()
main = writePng "/mnt/c/Users/moa17/Desktop/test.png" $ snd $ uncurry (generateFoldImage renderAtPixel (Screen 100 100 25, world, outputSize)) outputSize

world = [
  Box (Vector 0 (-120) 40) 100 100 100, 
  Box (Vector (-100) (-20) (50)) 50 50 50,
  Sphere (Vector 20 10 50) 40
  ]