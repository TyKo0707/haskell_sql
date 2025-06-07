import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import CSVReader (loadAndDescribeCSV, inferColumnTypes)
import QueryParser (parseSQLQuery, SQLQuery(..))
import QueryValidator (loadAndValidateQuery)
import QueryExecutor (runQuery)
import OutputWriter (writeCSV)

import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv as Csv

type Header = V.Vector B.ByteString
type Rows = V.Vector Csv.NamedRecord

main :: IO ()
main = do
    args <- getArgs
    case args of
        -- Auto pipeline mode
        (csvPath : query : outputPath : _) -> do
            putStrLn "You are working in auto mode"
            putStrLn $ "CSV file path: " ++ csvPath
            putStrLn $ "Query: " ++ query
            putStrLn $ "Output will be saved at: " ++ outputPath

            result <- loadAndDescribeCSV csvPath
            case result of
                Nothing -> putStrLn "Failed to load CSV file."
                Just (header, rows) -> case parseSQLQuery query of
                    Left err -> putStrLn $ "Query parsing failed:\n" ++ err
                    Right parsed -> do
                        loadAndValidateQuery result query
                        let colTypes = inferColumnTypes header rows
                        let resultRows = runQuery parsed rows colTypes
                        -- mapM_ print resultRows
                        let selectedHeader = V.fromList $ map B.pack (selectCols parsed)
                        writeCSV outputPath selectedHeader resultRows

        -- Interactive mode
        (csvPath : _) -> do
            putStrLn "You are working in interactive mode"
            putStrLn $ "CSV file path: " ++ csvPath

            result <- loadAndDescribeCSV csvPath

            putStr "\nEnter your SQL query: "
            hFlush stdout
            query <- getLine

            putStr "Enter output file path: "
            hFlush stdout
            outputPath <- getLine

            case result of
                Nothing -> putStrLn "Failed to load CSV file."
                Just (header, rows) -> case parseSQLQuery query of
                    Left err -> putStrLn $ "Query parsing failed:\n" ++ err
                    Right parsed -> do
                        loadAndValidateQuery result query
                        let colTypes = inferColumnTypes header rows
                        let resultRows = runQuery parsed rows colTypes
                        -- mapM_ print resultRows
                        let selectedHeader = V.fromList $ map B.pack (selectCols parsed)
                        writeCSV outputPath selectedHeader resultRows

        -- Default usage
        _ -> putStrLn "Usage:\n  Auto mode: ./sqlreader <csv> <query> <output>\n  Interactive mode: ./sqlreader <csv>"
