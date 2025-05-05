module Main ( main
            ) where

import Bot

main :: IO ()
main = readFile "/home/jose/api/telegram_bot" >>= run
