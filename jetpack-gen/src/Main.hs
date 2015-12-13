module Main where

import IDE.Gen

main :: IO ()
main = do
  putStrLn "start"
  jetpackGen
  putStrLn "end"

