module Main where

import Codec.Picture
import Vector
import Debug.Trace
import RayTracer

yellow = RGB 255 215 64 
white = RGB 255 255 255
green = RGB 24 142 112

g = -120
world = [
  -- SphericalLight white (Vector 0 300 0) 40
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
    images = [renderSingle p (960, 540) 1 | p <- [0]]

renderSingle :: Double -> (Int, Int) -> Int -> Image PixelRGB8
renderSingle p outputSize _ = snd $ uncurry (generateFoldImage renderAtPixel (Screen (160, 90, 100) (Vector 0 0 p), world, outputSize, smallerImage)) outputSize
  where
    smallerImage = Rec (0, 0) (0, 0) (renderImage p outputSize 0)

renderImage :: Double -> (Int, Int) -> Int -> Image PixelRGB8
renderImage p outputSize 0 = uncurry (generateImage renderBlank) outputSize
renderImage p outputSize d = snd $ uncurry (generateFoldImage renderAtPixel (Screen (160, 90, 100) (Vector 0 0 p), world, outputSize, smallerImage)) outputSize
  where
    smallerImage = Rec (0, 0) (480, 270) (renderImage p outputSize (d-1))

renderBlank :: Int -> Int -> PixelRGB8
renderBlank i j = PixelRGB8 0 255 0

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b