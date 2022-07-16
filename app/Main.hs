{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant ==" #-}
module Main where

import System.Environment
import System.Exit
import Data.Maybe
import Tab ( Tabdata, datainit, maketab )
import Lib
import Parseargv
import Dist
import Closest
import Kmeans

checktable :: Maybe [Tabdata] -> IO()
checktable Nothing = exitWith (ExitFailure 84)
checktable x = putStr ""

checklen :: [Tabdata] -> Int -> IO()
checklen tab x | length tab < x = exitWith (ExitFailure 84)
               | otherwise = putStr ""

main :: IO()
main = do
    args <- getArgs;
    let parsed = parseargv args parsedinit
    checkparsedargv parsed
    let p_fm = (fromMaybe parsedinit parsed)
    contents <- readFile (fromMaybe "" (path p_fm))
    let x = maketab (mysplit contents '\n')
    checktable x
    checklen (fromMaybe [datainit] x) (fromMaybe 0 (nbr_colors p_fm))
    kmeans (fromMaybe [datainit] x) (fromMaybe 0 (nbr_colors p_fm))
    putStr ""
