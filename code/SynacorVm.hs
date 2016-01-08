module Main where

main :: IO ()
main = do 
    contents <- getContents
    putStr contents
