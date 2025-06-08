module QueryValidator (validateQuery, loadAndValidateQuery) where

import QueryParser (parseSQLQuery, SQLQuery(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import qualified Data.Csv as Csv

type Header = V.Vector B.ByteString
type Rows = V.Vector Csv.NamedRecord

-- Extract columns names from header
extractColumnNames :: Header -> [String]
extractColumnNames = map B.unpack . V.toList

-- Validate query on loaded header and parsed query
loadAndValidateQuery :: Maybe (Header, Rows) -> String -> IO ()
loadAndValidateQuery result query =
    case result of
        Nothing -> return ()
        Just (header, _) -> do
            let columnNames = extractColumnNames header
            case parseSQLQuery query of
                Left err -> putStrLn $ "Query parsing error:\n" ++ err
                Right parsed -> case validateQuery columnNames parsed of
                    Left vErr -> putStrLn $ "Query validation failed:\n" ++ vErr
                    Right ()  -> putStrLn "\nQuery parsed and validated successfully."


validateQuery :: [String] -> SQLQuery -> Either String ()
validateQuery availableColumns (SQLQuery cols _ _ _ _) =
    let unknown = filter (`notElem` availableColumns) cols
    in if null unknown
        then Right ()
        else Left $ "Unknown columns: " ++ unwords unknown
