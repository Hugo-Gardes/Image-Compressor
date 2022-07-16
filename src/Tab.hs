module Tab where

import Lib
import Data.Maybe

data Tabdata = Tabdatat {
    pos :: [Int],
    red :: Double,
    green :: Double,
    blue :: Double
} deriving (Show, Eq)

datainit :: Tabdata
datainit = Tabdatat {pos = [0, 0], red = 0, green = 0, blue = 0}

getString :: String -> String
getString (',' : ' ' : xs) = xs
getString (')' : xs) = xs
getString ('(' : xs) = xs
getString (x : xs) = getString xs
getString xs = "Error"

getInt :: String -> Int
getInt x = fromMaybe 0 (readInt (getNumber x))

getFloat :: String -> Float
getFloat x = fromMaybe 0 (readFloat (getNumber x))

getDouble :: String -> Double
getDouble x = fromMaybe 0 (readDouble (getNumber x))

getNumber :: String -> String
getNumber (',': ' ' : xs) = ""
getNumber (',' : xs) = ""
getNumber (')': xs) = ""
getNumber ('(': xs) = ""
getNumber (x : xs) = x : getNumber xs
getNumber xs = "Error"

maketab :: [String] -> Maybe [Tabdata]
maketab [] = Just []
maketab ((x : xs) : xz) = Just (loop xs True False False False False datainit :
    fromMaybe [datainit] (maketab xz))
    where
        loop :: String -> Bool -> Bool -> Bool -> Bool -> Bool -> Tabdata ->
            Tabdata
        loop s False False True False False tab = loop s False True False True
            False tab{red = getDouble s}
        loop s False False False True False tab = loop s False True False False
            True tab{green = getDouble s}
        loop s False False False False True tab = loop s True False False False
            True tab{blue = getDouble s}
        loop s True False False False True tab = tab
        loop (',' : ' ' : xs) False True r g b tab = loop xs False False r g b
            tab
        loop (',' : xs) False True r g b tab = loop xs False False r g b
            tab
        loop ('(' : xs) False True False False False tab = loop xs False False
            True False False tab
        loop (x : xs) False True r g b tab = loop xs False True r g b tab
        loop (x : xs) True False False False False tab = loop str False True
            False False False tab{pos = [getInt (x : xs), getInt str]}
        loop xs x y z a w c = datainit
        str = getString xs
maketab xs = Nothing
