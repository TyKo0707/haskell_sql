module Main where

import ArgsTest (runArgTests)

main :: IO ()
main = do
    putStrLn "\nRUNNING ALL TESTS"
    runArgTests
