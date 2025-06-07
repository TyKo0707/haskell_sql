module Main where

import ArgsTest (runArgTests)
import CSVTest (runCSVTests)
import QueryTest (runQueryTests)
import QuerySystemTest (runQuerySystemTests)

main :: IO ()
main = do
    putStrLn "\nRUNNING ALL TESTS"
    runArgTests
    runCSVTests
    runQueryTests
    runQuerySystemTests
