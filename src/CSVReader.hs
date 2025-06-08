module CSVReader (loadCSV, inferColumnTypes, loadAndDescribeCSV) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

type Row = Csv.NamedRecord

-- loadCSV reads a csv file from the disk and decodes it with headers
loadCSV :: FilePath -> IO (Either String (Csv.Header, V.Vector Row))
loadCSV path = do
    content <- BL.readFile path
    case Csv.decodeByName content of
        Left err -> return $ Left err
        Right (header, rows) -> return $ Right (header, rows)

-- This one tries to figure out what type each column has (used also for query execution later)
inferColumnTypes :: Csv.Header -> V.Vector Row -> [(String, String)]
inferColumnTypes header rows =
    let firstTenRows = take 10 $ V.toList rows
        columns = V.toList header
    in map (\col -> (B.unpack col, guessType col firstTenRows)) columns

guessType :: B.ByteString -> [Row] -> String
guessType col rows =
    let values = mapMaybe (HM.lookup col) rows
        asText = map B.unpack values
    in if all isInt asText then "Int"
       else if all isFloat asText then "Float"
       else "String"

-- checks if given string is int
isInt :: String -> Bool
isInt s = case reads s :: [(Int, String)] of [(n, "")] -> True; _ -> False

-- checks if given string is float
isFloat :: String -> Bool
isFloat s = case reads s :: [(Float, String)] of [(n, "")] -> True; _ -> False

-- Download csv file from path and write columns along with types
loadAndDescribeCSV :: FilePath -> IO (Maybe (Csv.Header, V.Vector Row))
loadAndDescribeCSV csvPath = do
    -- load csv from a file and process result (either Left or Right)
    dataset <- loadCSV csvPath
    case dataset of
        Left err -> do
            putStrLn $ "Error loading CSV: " ++ err
            return Nothing
        Right (header, rows) -> do
            putStrLn $ "Successfully loaded " ++ show (V.length rows) ++ " rows from file.\n"
            if V.null rows
                then putStrLn "No data in CSV file."
                else do
                    let types = inferColumnTypes header rows
                    putStrLn "Columns and inferred types:"
                    mapM_ (\(h, t) -> putStrLn $ "  " ++ h ++ " :: " ++ t) types
            return $ Just (header, rows)
