module Dist where

import Lib
import Tab

import Data.Maybe

getfrommaybe :: Maybe Int -> Float
getfrommaybe x = fromIntegral (fromMaybe 0 x) :: Float

dist :: Tabdata -> Tabdata -> Double
dist tab1 tab2 = sqrt res
    where
        r = (red tab2 - red tab1)**2
        g = (green tab2 - green tab1)**2
        b = (blue tab2 - blue tab1)**2
        res = r + g + b