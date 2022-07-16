module Kmeans where

import Tab
import Closest
import Dist
import System.Random
import System.IO.Unsafe

replacetabdata :: [[Tabdata]] -> [Tabdata] -> Int -> [[Tabdata]]
replacetabdata (x:xs) tab 0 = tab : xs
replacetabdata (x:xs) tab index = x : replacetabdata xs tab (index - 1)
replacetabdata c x b = c

cluster :: [Tabdata] -> [Tabdata] -> [[Tabdata]] -> [[Tabdata]]
cluster x [] z = z
cluster x (y:ys) z = loopcluster x (y:ys) z finalpoint
    where
        finalpoint :: Int
        finalpoint = comparedist x y (dist (x !! 0) y) 0 0
        loopcluster :: [Tabdata] -> [Tabdata] -> [[Tabdata]]
            -> Int -> [[Tabdata]]
        loopcluster x (y:ys) z i = cluster x ys (replacetabdata z
            (y : z !! i) i)

comparedist :: [Tabdata] -> Tabdata -> Double -> Int -> Int -> Int
comparedist [] y d i p = p
comparedist (x:xs) y d i p
            | dist x y < d = comparedist xs y (dist x y) (i + 1) i
            | otherwise = comparedist xs y d (i + 1) p

takek :: Int -> [Tabdata] -> [Tabdata] -> [Tabdata]
takek 0 tab l = l
takek k tab l = takek (k - 1) tab (getrand l tab)

isintab :: [Tabdata] -> Tabdata -> Bool
isintab [] x = False
isintab (x : xs) var | x == var = True
                     | otherwise = isintab xs var

getrand :: [Tabdata] -> [Tabdata] -> [Tabdata]
getrand already tab | r `elem` already = getrand already tab
                    | otherwise = r : already
    where
        len = length tab - 1
        r = (tab !! (getRandomInt 0 len))
getRandomInt :: Int -> Int -> Int
getRandomInt x y = unsafePerformIO (getStdRandom (randomR (x, y)))

printpixels :: Tabdata -> String
printpixels pixels = "(" ++ r ++ "," ++ g ++ "," ++ b ++ ")"
    where
        r = show (round (red pixels)) :: String
        g = show (round (green pixels)) :: String
        b = show (round (blue pixels)) :: String

printpos :: Tabdata -> String
printpos tab = "(" ++ x ++ "," ++ y ++ ")"
    where
        x = show (pos tab !! 0) :: String
        y = show (pos tab !! 1) :: String

printcluster :: [Tabdata] -> IO()
printcluster [] = putStr ""
printcluster xs = putStrLn ((printpos x) ++ " " ++ (printpixels x)) >> end
    where
        x = head xs
        end :: IO()
        end = printcluster (tail xs)

printclusters :: [[Tabdata]] -> IO()
printclusters [] = putStr ""
printclusters xs = tir >> putStrLn (closest tab) >> putStrLn "-" >>
    printcluster tab >> printclusters e
    where
        tir = putStrLn "--"
        tab = head xs
        e = tail xs

kmeans :: [Tabdata] -> Int -> IO()
kmeans x i = printclusters (cluster (takek i x []) x (maketab [] i))
    where
        maketab :: [[Tabdata]] -> Int -> [[Tabdata]]
        maketab x 0 = x
        maketab x k = maketab ([] : x) (k - 1)