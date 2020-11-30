module Main where

import Control.Concurrent.Async
import System.Environment
import Data.List
import Debug.Trace
import Codec.Picture
import Vector
import RayTracer

-- default ground y-level
g = -120

main :: IO ()
main = do
  args <- getArgs
  let resolution = setResolution args
      shadow = "--shadow" `elem` args
      useNoShadowZoom = "--noshadow-zoom" `elem` args
      frames = setFrames args
  debugNotice args (resolution, shadow, useNoShadowZoom, frames)
  -- lights can only have a colour
  let lamp = PixelRGB8 231 227 216
      bluelight = PixelRGB8 236 243 255
      sun = PixelRGB8 246 209 0
  -- material can be both colour and texture (loaded with getTexture)
  let white = Colour (PixelRGB8 255 255 255)
      board = Colour (PixelRGB8 101 188 80)
      silver = Colour (PixelRGB8 197 197 197)
      darksilver = Colour (PixelRGB8 124 124 124)
      blue = Colour (PixelRGB8 136 175 238)
      red = Colour (PixelRGB8 204 51 51)
      brown = Colour (PixelRGB8 73 2 0)
      brown2 = Colour (PixelRGB8 108 79 81)
      box = Colour (PixelRGB8 177 142 90)
  edinburgh <- getTexture "textures/edinburgh.jpg"
  wood <- getTexture "textures/wood.jpg"
  face <- getTexture "textures/face.png"
  zoom <- getTexture ("textures/zoom-" ++ (if shadow && (not useNoShadowZoom) then [] else "no") ++ "shadow.png")
  marble <- getTexture "textures/marble.jpg"
  clothes <- getTexture "textures/clothes.png"
  postit <- getTexture "textures/ilaquiz.png"
  mamazon <- getTexture "textures/mamazon.png"
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

        Box box (Vector (-140) g 185) 100 60 60,
        Box mamazon (Vector (-140) g 184.9) 100 60 0.1,

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
        
        Box marble (Vector 110 (g+20) 200) 2 30 50,
        Box marble (Vector 158 (g+20) 200) 2 30 50,
        Box marble (Vector 112 (g+20) 195) 2 30 50,
        Box marble (Vector 156 (g+20) 195) 2 30 50,
        Box marble (Vector 112 (g+20) 248) 46 30 2,
        Box marble (Vector 112 (g+20) 195) 46 30 2,
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

        -- mug 
        Cylinder red (Vector 120 (g+84) 220) 3.5 10,
        Cylinder red (Vector 126 (g+88) 220) 0.5 4,
        Box red (Vector 123.5 (g+87.5) 219.5) 2.5 1 1,
        Box red (Vector 123.5 (g+92) 219.5) 2.5 1 1,

        Box board (Vector 90 (g+120) 250) 70 80 2,
        Box postit (Vector 100 (g+140) 249.8) 10 10 0.2,

        -- bookshelf
        Box wood (Vector 170 g 220) 100 3 30,
        Box wood (Vector 170 (g+297) 220) 100 3 30,
        Box wood (Vector 170 (g+3) 220) 3 294 30,
        Box wood (Vector 267 (g+3) 220) 3 294 30,

        -- books
        Box wood (Vector 173 (g+33) 220) 94 3 30,
        Box wood (Vector 173 (g+66) 220) 94 3 30,
        Box wood (Vector 173 (g+100) 220) 94 3 30,
        
        Box brown (Vector 173 (g+103) 225) 4 20 27,
        Box brown2 (Vector 178 (g+103) 225) 3 30 27,
        Box brown (Vector 181 (g+103) 223) 3 23 20,

        Box wood (Vector 173 (g+150) 220) 94 3 30,
        Box wood (Vector 173 (g+180) 220) 94 3 30,
        Box wood (Vector 173 (g+210) 220) 94 3 30,
        Box wood (Vector 173 (g+250) 220) 94 3 30,
        Box wood (Vector 173 (g+280) 220) 94 3 30,

        -- face
        Ellipsoid face (Vector (-5) (g+120) 40) 7.5 12 10,
        -- body
        Ellipsoid clothes (Vector (-5) (g+70) 40) 15 50 10
        ]
  -- illuminations
  let lights = [
        SphericalLight lamp 80 (Vector 200 (g+290) 100) 40,
        SphericalLight lamp 80 (Vector (-200) (g+290) 100) 40,
        SphericalLight bluelight 10 (Vector 0 0 (-60)) 30
        ] ++ (if shadow then [SphericalLight sun 500 (Vector 0 500 0) 0] else [])
  
  -- render the images for the provided positions
  -- camera needs to go from 0,0,0 to 91.7647275,3.4090909,226.75
  -- so, interpolate from 0..150 and divide each value accordingly
  let images = [
        renderSingle (Vector x y z) resolution (objects, lights) shadow
        | i <- frames,
          let j = (fromIntegral i),
          let x = j/(150/91.7647275),
          let y = j/(44), -- so much manual tweaking was done with these three values... ;( time gone
          let z = j/(150/226.75)]

  -- render each out as a png instead of GIF for two reasons:
  -- 1. I can visibly see the progress, useful for estimation (not possible with lazy eval monads)
  -- 2. I can stop halfway through and still use the data
  -- 3. I can combine into GIF using an external tool anyway
  mapConcurrently renderOut (zip images [0..])
  return ()

  -- convert to GIF and output it
  -- fromRight (return ()) $ writeGifAnimation "output.gif" 50 LoopingForever images

debugNotice :: [String] -> ((Int, Int), Bool, Bool, [Int]) -> IO ()
debugNotice args (res, sh, zm, frs) = 
  putStrLn ("Running SimpleRayTracerHS with args: " ++ intercalate " " args) >>
  putStrLn ("Output resolution: " ++  show (fst res) ++ "x" ++ show (snd res)) >>
  putStrLn ("Shadow Enabled: " ++ if sh then (if zm then "yes (no shadow Zoom)" else "yes (shadow Zoom)") else "no") >>
  putStrLn ("Frame Count: " ++ show (length frs))

setResolution :: [String] -> (Int, Int)
setResolution args
  | fromRes == [] = (960, 540)
  | otherwise = ((read . takeWhile (/= ':')) resolarg, (read . drop 1 . dropWhile (/= ':')) resolarg)
    where
      fromRes = dropWhile (/= "--res") args
      resolarg = head (drop 1 fromRes)

setFrames :: [String] -> [Int]
setFrames args
  | fromRes == [] = [0..150]
  | otherwise = map read $ splitAt ',' framearg
    where
      fromRes = dropWhile (/= "--frames") args
      framearg = head (drop 1 fromRes)
      splitAt :: Char -> String -> [String]
      splitAt c [] = []
      splitAt c xs = (takeWhile (/= c) xs) : splitAt c (drop 1 $ dropWhile (/= c) xs)

-- | Renders an image out to a file
renderOut :: (IO (Image PixelRGB8), Int) -> IO ()
renderOut (image, i) = do
  im <- image
  writePng ("output/" ++ (reverse . take 3 . reverse) ("000" ++ show i) ++ ".png") $ im  

-- | Renders a single frame from the given position of the camera, output resolution, and [objects, lights]
renderSingle :: Vector -> (Int, Int) -> World -> Bool -> IO (Image PixelRGB8)
renderSingle pos (x,y) world shadow = do 
  -- instead of using waitImage which takes an IO for pixel, generate all the pixels asyncrhonously first
  -- then use generateImage for lists
  pixels <- mapConcurrently (uncurry (renderPixel pos (x,y) world shadow)) [(j, i) | j <- [0..(y-1)], i <- [0..(x-1)]]
  pixels2 <- mapConcurrently wait pixels
  return $ generateImage (getPixelFrom (x,y) pixels2) x y

renderPixel :: Vector -> (Int, Int) -> World -> Bool -> Int -> Int -> IO (Async PixelRGB8)
renderPixel pos size world shadow j i = async $ renderAtPixel (Screen (1.7777, 1, 1) pos, world, size, shadow) i j

getPixelFrom :: (Int, Int) -> [PixelRGB8] -> Int -> Int -> PixelRGB8
getPixelFrom (x,y) pixels i j = head $ drop i (drop (j*x) pixels)

-- | Taken from `fromRight` in the newer versions of Prelude 
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b