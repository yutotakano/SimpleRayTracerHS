module Main where

import Codec.Picture
import Vector
import RayTracer

outputSize = (512, 512)

main :: IO ()
-- main = writePng "/mnt/c/Users/moa17/Desktop/test.png" $ snd $ uncurry (generateFoldImage renderAtPixel (Screen 200 200 100, world, outputSize)) outputSize

main = fromRight (return ()) $ writeGifAnimation "/mnt/c/Users/moa17/Desktop/test.gif" 100 LoopingForever images
  where
    images = [snd $ uncurry (generateFoldImage renderAtPixel (Screen (200, 200, 100) (Vector 0 0 0), world, outputSize)) outputSize | p <- [1]]

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

yellow = RGB 255 215 64 
white = RGB 255 255 255
green = RGB 24 142 112

world = [
  Box white (Vector (-500) (60) 0) 1000 10 1000,
  Box white (Vector 0 50 30) 20 100 20,
  Box white (Vector 0 50 60) 20 100 20,
  Box white (Vector 100 50 30) 20 100 20,
  Box white (Vector 100 50 60) 20 100 20, 
  Box yellow (Vector (-100) (-20) (40)) 50 50 50,
  Sphere green (Vector 20 10 30) 20,
  Sphere white (Vector 20 (-20) 30) 15
  ]