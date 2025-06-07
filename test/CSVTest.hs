module CSVTest (runCSVTests) where

import qualified Data.Vector as V
import qualified Data.Csv as Csv
import qualified Data.ByteString.Char8 as B

import CSVReader (loadCSV)
import TestFramework

runCSVTests :: IO ()
runCSVTests = do
    putStrLn "\nRunning CSVTest..."

    let testFile = "./test/test_data/test1.csv"

    result <- loadCSV testFile
    case result of
        Left err -> assertFailure ("Failed to load CSV: " ++ err)
        Right (header, rows) -> do
            assertEqual "Header contains correct columns"
                ["id", "name", "score"]
                (map B.unpack $ V.toList header)

            assertEqual "Number of rows is correct"
                3
                (V.length rows)
