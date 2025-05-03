module Main ( main
            ) where

import Lib

main :: IO ()
main = putStrLn . show . f $ 5
