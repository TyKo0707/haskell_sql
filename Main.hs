import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import CSVReader (loadAndDescribeCSV)

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

            _ <- loadAndDescribeCSV csvPath
            return ()

        -- Interactive mode
        (csvPath : _) -> do
            putStrLn "You are working in interactive mode"
            putStrLn $ "CSV file path: " ++ csvPath

            _ <- loadAndDescribeCSV csvPath

            putStr "\nEnter your SQL query: "
            hFlush stdout
            query <- getLine

            putStr "Enter output file path: "
            hFlush stdout
            outputPath <- getLine

            putStrLn $ "Query: " ++ query
            putStrLn $ "Output will be saved at: " ++ outputPath

        -- Default message
        _ -> putStrLn "Usage:\n  Auto mode: ./sqlreader <csv> <query> <output>\n  Interactive mode: ./sqlreader <csv>"
