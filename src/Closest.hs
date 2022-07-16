module Closest where

import Tab

data Pixels = Pixels {
    rouge :: Double,
    vert :: Double,
    bleu :: Double
}

pixelsbase :: Pixels
pixelsbase = Pixels {rouge = 0, vert = 0, bleu = 0}

makeString :: Pixels -> String
makeString pixels = "(" ++ r ++ "," ++ g ++ "," ++ b ++ ")"
    where
        r = show (round (rouge pixels)) :: String
        g = show (round (vert pixels)) :: String
        b = show (round (bleu pixels)) :: String

calcpixels :: [Tabdata] -> Pixels -> Double-> Pixels
calcpixels (x : xs) pixels nbr = calcpixels xs np (nbr + 1)
    where
        r = rouge pixels + red x
        g = vert pixels + green x
        b = bleu pixels + blue x
        np :: Pixels
        np = (pixels {rouge = r, vert = g, bleu = b})
calcpixels x c n = means c n

means :: Pixels -> Double -> Pixels
means c 0.0 = Pixels{rouge = -1, bleu = -1, vert = -1}
means c n = c {rouge = r, vert = g, bleu = b}
    where
        r = fromIntegral (round (rouge c / n))
        g = fromIntegral (round (vert c / n))
        b = fromIntegral (round (bleu c / n))

closest :: [Tabdata] -> String
closest x = makeString (calcpixels x pixelsbase 0)

--  outil profiling