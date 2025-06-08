-- basic imports
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv as Csv
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

-- imports from src/
import CSVReader (loadAndDescribeCSV, inferColumnTypes)
import QueryParser (parseSQLQuery, SQLQuery(..))
import QueryValidator (loadAndValidateQuery)
import QueryExecutor (runQuery)
import OutputWriter (writeCSV)

-- Ttype aliases
type Header = V.Vector B.ByteString
type Rows = V.Vector Csv.NamedRecord

processCSVQuery :: Maybe (Header, Rows) -> String -> FilePath -> IO ()
processCSVQuery dataset query outputPath =
    case dataset of
        Nothing -> putStrLn "Failed to load CSV file."
        -- Just is a constructor for Maybe type
        Just (header, rows) -> case parseSQLQuery query of -- (3.1)
            Left err -> putStrLn $ "Query parsing failed:\n" ++ err
            Right parsed -> do
                loadAndValidateQuery dataset query -- (3.2)
                let colTypes = inferColumnTypes header rows
                let resultRows = runQuery parsed rows colTypes -- (4)
                let filterHeader = V.fromList $ map B.pack (selectCols parsed)
                writeCSV outputPath filterHeader resultRows -- (5)

-- General steps here are: (1) process args and determine mode -> (2) load data from csv -> 
-- (3) parse and validate query -> (4) execute query -> (5) write resulted output to the file
main :: IO ()
main = do
    args <- getArgs -- (1)
    case args of
        -- Auto pipeline mode: all arguments are provided from the beginning
        (csvPath : query : outputPath : _) -> do
            putStrLn "You are working in auto mode"
            putStrLn $ "CSV file path: " ++ csvPath
            putStrLn $ "Query: " ++ query
            putStrLn $ "Output will be saved at: " ++ outputPath

            dataset <- loadAndDescribeCSV csvPath -- (2)
            processCSVQuery dataset query outputPath -- (3):(5)

        -- Interactive mode: ask for query and output path explicitly
        (csvPath : _) -> do
            putStrLn "You are working in interactive mode"
            putStrLn $ "CSV file path: " ++ csvPath

            dataset <- loadAndDescribeCSV csvPath -- (2)

            putStr "\nEnter your SQL query: "
            hFlush stdout
            query <- getLine

            putStr "Enter output file path: "
            hFlush stdout
            outputPath <- getLine

            processCSVQuery dataset query outputPath -- (3):(5)

        -- Default message
        _ -> putStrLn "Usage:\n  Auto mode: ./sqlreader <csv> <query> <output>\n  Interactive mode: ./sqlreader <csv>"
