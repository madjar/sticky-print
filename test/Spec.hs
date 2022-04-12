module Main (main) where

import StickyPrint (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
