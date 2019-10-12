module Main where

import System.IO

main =
    let
        average x = fromIntegral (sum x) / fromIntegral (length x)
        fixPoint p f = fromIntegral (round (f * 10^p)) / 10^p
    in
        getContents >>= print . fixPoint 2 . average . (length . words <$>) . lines
