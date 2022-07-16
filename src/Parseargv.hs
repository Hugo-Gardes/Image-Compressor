module Parseargv where

import Data.Maybe

import Lib
import System.Exit

data Parseargvs = Parseargvs {
    nbr_colors :: Maybe Int,
    convergence_limit :: Maybe Float,
    path :: Maybe String
} deriving (Show)

parsedinit :: Parseargvs
parsedinit = Parseargvs {nbr_colors = Nothing, convergence_limit = Nothing,
    path = Nothing}

parseargv :: [String] -> Parseargvs -> Maybe Parseargvs
parseargv [] w = Just w
parseargv ("-n" : y : xs) w = parseargv xs w{nbr_colors = readInt y}
parseargv ("-l" : y : xs) w = parseargv xs w{convergence_limit = readFloat y}
parseargv ("-f" : y : xs) w = parseargv xs w{path = Just y}
parseargv x w = Nothing

checkparsedargv :: Maybe Parseargvs -> IO ()
checkparsedargv Nothing = exitWith (ExitFailure 84)
checkparsedargv x | isNothing (path (fromMaybe parsedinit x)) =
    putStrLn "path" >> exitWith
    (ExitFailure 84)
        | isNothing (convergence_limit (fromMaybe parsedinit x)) =
          putStrLn "limit == " >> exitWith (ExitFailure 84)
        | isNothing (nbr_colors (fromMaybe parsedinit x)) =
          putStrLn "colors" >> exitWith (ExitFailure 84)
        | otherwise = putStr ""