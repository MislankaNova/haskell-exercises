-- Cellular automata

import System.IO
import System.Random
import Data.List
import Data.Maybe

type Line = [Char]

type Rule = [Char] -> Char

binary :: Int -> [Char]
binary x
  | x == 0    = " "
  | x == 1    = "x"
  | even x    = (binary (div x 2)) ++ " "
  | otherwise = (binary (div x 2)) ++ "x"

padLeft :: Int -> a -> [a] -> [a]
padLeft x a l =
  (take (x - length l) (repeat a)) ++ l

rule :: Int -> Rule
rule x cs =
  fromJust (lookup cs combs)
  where
    digs  = ['x', ' ']
    bins  = [[a, b, c] | a <- digs, b <- digs, c <- digs]
    combs = zip bins (padLeft 8 ' ' (binary x))

genLine :: Rule -> Line -> Line
genLine f (c : c' : c'' : cs) =
  (f [c, c', c'']) : (genLine f (c' : c'' : cs))

oneLine :: Rule -> Line -> Line
oneLine f l = 
  ' ' : genLine f l

showAutomata :: Int -> Int -> Int -> Line -> [Line]
showAutomata r t d l =
  take 2000 (map ((take t) . (drop d)) (iterate (oneLine (rule r)) l))

firstLine :: [Char]
firstLine = cycle "x x"

main =
  do
    putStr "Rule number: " >> hFlush stdout
    num <- getLine
    rule <- return (read num :: Int)
    putStr "Line width: " >> hFlush stdout
    num <- getLine
    t <- return (read num :: Int)
    putStr "Drop: " >> hFlush stdout
    num <- getLine
    d <- return (read num :: Int)
    ----------
    seed <- newStdGen
    let rs = randomInit seed
    init <- return (concat (map binary rs))
    automata <- return (showAutomata rule t d init)
    putStr (unlines automata)

randomInit :: StdGen -> [Int]
randomInit = unfoldr (\g -> Just (randomR (0, 1) g))
