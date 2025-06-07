module Main where

import ArgsTest (runArgTests)
import CSVTest (runCSVTests)
import QueryTest (runQueryTests)

main :: IO ()
main = do
    putStrLn "\nRUNNING ALL TESTS"
    runArgTests
    runCSVTests
    runQueryTests
