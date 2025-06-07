module Main where

import ArgsTest (runArgTests)
import CSVTest (runCSVTests)

main :: IO ()
main = do
    putStrLn "\nRUNNING ALL TESTS"
    runArgTests
    runCSVTests
