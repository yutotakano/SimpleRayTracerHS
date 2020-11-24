module Main where

import Codec.Picture
import Vector
import Debug.Trace
import RayTracer

lamp = RGB 231 227 216
yellow = RGB 255 215 64 
white = RGB 255 255 255
green = RGB 24 142 112

g = -120
lights = [
  SphericalLight lamp (Vector 0 300 0) 40,
  SphericalLight green (Vector 0 0 (-50)) 40]
objects = [
  -- ground
  Plane white (Vector 0 1 0) (Vector 0 g 0)
  ] ++ [
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


main :: IO ()
main = fromRight (return ()) $ writeGifAnimation "output.gif" 5 LoopingForever images
  where
    images = [renderSingle p (960, 540) | p <- [0..30]]

renderSingle :: Double -> (Int, Int) -> Image PixelRGB8
renderSingle p outputSize = snd $ uncurry (generateFoldImage renderAtPixel (Screen (160, 90, 100) (Vector 0 0 p), (objects, lights), outputSize)) outputSize

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b