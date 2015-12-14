module Main where

import           Gen

main :: IO ()
main = do
  putStrLn "start"
  jetpackGen
  putStrLn "end"
