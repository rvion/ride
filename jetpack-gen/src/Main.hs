module Main where

import IDE.JetpackGen

main :: IO ()
main = do
  putStrLn "start"
  jetpackGen
  putStrLn "end"

