module Main where

import Lib

main :: IO ()
main = do  
  putStrLn "\nEntre com dia mes e ano (separados por espaco):"
  input <- getLine  
  putStrLn (diaDoCuringa input)  