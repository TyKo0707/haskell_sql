module CSVReader (loadCSV, inferColumnTypes, loadAndDescribeCSV) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

type Row = Csv.NamedRecord

loadCSV :: FilePath -> IO (Either String (Csv.Header, V.Vector Row))
loadCSV path = do
    content <- BL.readFile path
    case Csv.decodeByName content of
        Left err -> return $ Left err
        Right (header, rows) -> return $ Right (header, rows)

inferColumnTypes :: Csv.Header -> V.Vector Row -> [(String, String)]
inferColumnTypes header rows =
    let sampleRows = take 10 $ V.toList rows
        columns = V.toList header
    in map (\col -> (B.unpack col, guessType col sampleRows)) columns

guessType :: B.ByteString -> [Row] -> String
guessType col rows =
    let values = mapMaybe (HM.lookup col) rows
        asText = map B.unpack values
    in if all isInt asText then "Int"
       else if all isFloat asText then "Float"
       else "String"

isInt :: String -> Bool
isInt s = case reads s :: [(Int, String)] of [(n, "")] -> True; _ -> False

isFloat :: String -> Bool
isFloat s = case reads s :: [(Double, String)] of [(n, "")] -> True; _ -> False

-- Download csv file from path and write columns along with types
loadAndDescribeCSV :: FilePath -> IO (Maybe (Csv.Header, V.Vector Row))
loadAndDescribeCSV csvPath = do
    result <- loadCSV csvPath
    case result of
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
