module QuerySystemTest (runQuerySystemTests) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import System.Directory (removeFile)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

import QueryParser (parseSQLQuery, selectCols)
import QueryExecutor (runQuery)
import CSVReader (loadCSV, inferColumnTypes)
import OutputWriter (writeCSV)
import TestFramework

runQuerySystemTests :: IO ()
runQuerySystemTests = do
    putStrLn "\nRunning QuerySystemTest..."

    let testFile = "./test/test_data/movies.csv"
    csvResult <- loadCSV testFile

    case csvResult of
        Left err -> assertFailure ("CSV load failed: " ++ err)
        Right (header, rows) -> do
            let colTypes = inferColumnTypes header rows

            runQueryTest "SELECT Series_Title, Released_Year FROM data WHERE Released_Year > 2015"
                ["Dunkirk", "Tenet", "Parasite", "The Handmaiden", "Decision to Leave", "Soul", "Coco"]
                header rows colTypes

            runQueryTest "SELECT Series_Title FROM data WHERE Director = 'Pete Docter'"
                ["Soul", "Inside Out", "Up"]
                header rows colTypes

            runQueryTest "SELECT Series_Title FROM data WHERE IMDB_Rating >= 8.5"
                ["Inception", "Interstellar", "The Dark Knight", "Parasite"]
                header rows colTypes

            runQueryTest "SELECT Series_Title FROM data WHERE Released_Year < 2010"
                ["The Dark Knight", "Memories of Murder", "Mother", "Oldboy", "Up"]
                header rows colTypes

            runQueryTest "SELECT Series_Title FROM data WHERE Released_Year > 2015 ORDER BY Released_Year"
                ["The Handmaiden","Dunkirk","Coco","Parasite","Tenet","Soul","Decision to Leave"]
                header rows colTypes

            runQueryTest "SELECT Series_Title FROM data WHERE Released_Year > 2015 ORDER BY Released_Year LIMIT 3"
                ["The Handmaiden","Dunkirk","Coco"]
                header rows colTypes

runQueryTest :: String -> [String] -> Csv.Header -> V.Vector Csv.NamedRecord -> [(String, String)] -> IO ()
runQueryTest rawQuery expected header rows colTypes =
    case parseSQLQuery rawQuery of
        Left perr -> assertFailure ("SQL parse failed: " ++ perr)
        Right parsed -> withSystemTempFile "query_output.csv" $ \tempPath handle -> do
            hClose handle
            let result = runQuery parsed rows colTypes
                selectedHeader = V.fromList $ map B.pack (selectCols parsed)
            writeCSV tempPath selectedHeader result

            outputResult <- loadCSV tempPath
            case outputResult of
                Left err -> assertFailure ("Failed to reload written CSV: " ++ err)
                Right (_, outRows) -> do
                    let titles = map (B.unpack . extractField "Series_Title") (V.toList outRows)
                    assertEqual ("Query: " ++ rawQuery) expected titles

extractField :: String -> Csv.NamedRecord -> B.ByteString
extractField fieldName row = fromMaybe (B.pack "") (HM.lookup (B.pack fieldName) row)
