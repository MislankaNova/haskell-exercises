-- Perlin noise generator
-- Using the 'Vanilla' Perlin noise algorithm
-- Generates a map
-- And prints the map using ASCII characters

-- By MislankaNova
-- 14 Nov 2016

import System.Environment
import System.Random
import Data.List

------------
-- Real Part
------------

-- I have to implement my own vector
-- Since there is no cabal on college computers..
type Vector2 = (Double, Double)

-- Integer coordinate
type Coord = (Int, Int)

-- First for size, second for reso
type Meta = (Int, Int)

data Point = Point Double
           | EndOfLine

-- Gives the character representation of a point
displayPoint :: Point -> Char
displayPoint EndOfLine = '\n'
displayPoint (Point h)
  | h < 0.2    = ' '
  | h < 0.4    = '.'
  | h < 0.6    = '-'
  | h < 0.8    = '~'
  | otherwise  = '^'

-- Normalise a vector so that it has unit length
normalise :: Vector2 -> Vector2
normalise (x, y) =
  (x / v, y / v)
  where
    v = sqrt (x * x + y * y)

-- Create a list of normalised vectors using a seed
randomVectors :: Int -> [Vector2]
randomVectors s =
  zipWith ((normalise .) . (,)) rs rs'
  where
    rs  = randomRs (-10, 10) (mkStdGen s)
    rs' = randomRs (-10, 10) (mkStdGen (s + magick))
    magick = 27

----------

-- Gives the decimal part of a real number
dec :: Double -> Double
dec x = x - (fromIntegral (truncate x))

-- Splits a list of infinite length to an infinite list of lists of length n
splitAll :: Int -> [a] -> [[a]]
splitAll n l =
  lh : (splitAll n lr)
  where
    (lh, lr) = splitAt n l

-- Linear interpolation
lerp :: Double -> Double -> Double -> Double
lerp y y' dx =
  y + dx * (y' - y)

-- Calculate the influence of a nearby tile corner on a given coordinate
calculateInfluence :: Vector2 -> Coord -> Vector2 -> Double
calculateInfluence (x, y) (x', y') (u, v) =
  (x - fromIntegral x') * u + (y - fromIntegral y') * v

-- Calculate the "Height" of a certain point
calculatePoint :: [[Vector2]] -> Vector2 -> Point
calculatePoint vs p@(x, y) =
  Point ((lerp (lerp i0 i1 (dec x)) (lerp i2 i3 (dec x)) (dec y)) + 0.5)
  where
    x' = floor x
    y' = floor y
    i0 = calculateInfluence p (x'    , y'    ) (vs !!  y'      !!  x'     )
    i1 = calculateInfluence p (x' + 1, y'    ) (vs !!  y'      !! (x' + 1))
    i2 = calculateInfluence p (x'    , y' + 1) (vs !! (y' + 1) !!  x'     )
    i3 = calculateInfluence p (x' + 1, y' + 1) (vs !! (y' + 1) !! (x' + 1))

-- Generates a horizontal row of characters on the map
makeLine :: Meta -> [[Vector2]] -> Double -> [Point]
makeLine (reso, size) vs y =
  EndOfLine : (map (calculatePoint vs) (map (flip (,) y) xs))
  where
    xs   = take (reso * size) [0, step..]
    step = 1 / (fromIntegral reso :: Double)

-- Generate the entire map
makePoints :: Meta -> [[Vector2]] -> [Point]
makePoints (reso, size) vs =
  tail (concat lines)
  where
    lines = map (makeLine (reso, size) vs) ys
    ys    = take (reso * size) [0, step..]
    step  = 1 / (fromIntegral reso :: Double)

-----------------
-- Main functions
-----------------

main :: IO ()
main =
  do
    args <- getArgs
    case args of 
      a1 : a2 : a3 : [] -> parse args
      otherwise         -> help

help :: IO ()
help = 
  do
    putStrLn "Usage: perlin-ascii [SIZE] [RESOLUTION] [SEED]"
    putStrLn "Draws a map in ASCII characters generated using Perlin noise."
    putStrLn ""
    putStrLn "          [SIZE] : The length of the sides of the map."
    putStrLn "    [RESOLUTION] : The number of 'pixels' per tile."
    putStrLn "          [SEED] : The seed for pseudo-random generation."

parse :: [String] -> IO ()
parse (a1 : a2 : a3 : []) =
  case (p1, p2, p3) of
    ((p1', _) : _, (p2', _) : _, (p3', _) : _) -> generate (p1', p2', p3')
    otherwise                                  -> help
  where
    p1 = reads a1 :: [(Int, String)]
    p2 = reads a2 :: [(Int, String)]
    p3 = reads a3 :: [(Int, String)]

generate :: (Int, Int, Int) -> IO ()
generate (size, reso, seed) =
  do
    let meta = (reso, size)
    let vs = splitAll (size + 2) (randomVectors seed)
    let points = makePoints meta vs
    putStrLn (map displayPoint points)
