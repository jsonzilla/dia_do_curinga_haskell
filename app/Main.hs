module Main where

import Lib

main :: IO ()
-- main = ddc
main = do  
  putStrLn "Entre com dia mes e ano (separados por enter) e precione ctrl-c"
  putStrLn "\n\tEntre com dia mes e ano (separados por espaco):"
  input <- getLine  
  putStrLn (diaDoCuringa input)  