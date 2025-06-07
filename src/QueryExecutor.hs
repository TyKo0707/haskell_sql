module QueryExecutor (runQuery) where

import QueryParser (SQLQuery(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.List (sortOn, isPrefixOf)

import Data.Maybe (fromMaybe)

type Row = Csv.NamedRecord

-- Run SQLQuery on dataset and return filtered rows
runQuery :: SQLQuery -> V.Vector Row -> [(String, String)] -> [Row]
runQuery query rows types =
    let filtered = maybe (V.toList rows)
                         (\clause -> filter (evalWhere clause types) (V.toList rows))
                         (whereClause query)
        ordered = maybe filtered (\col -> sortOn (getValue col) filtered) (orderBy query)
        limited = maybe ordered (`take` ordered) (limitCount query)
    in selectColumns (selectCols query) (V.fromList limited)

-- Select only specified columns
selectColumns :: [String] -> V.Vector Row -> [Row]
selectColumns cols =
    V.toList . V.map (\row -> HM.filterWithKey (\k _ -> B.unpack k `elem` cols) row)

-- Evaluates WHERE clause based on inferred column types
evalWhere :: String -> [(String, String)] -> Row -> Bool
evalWhere clause types row =
    case parseClause clause of
        Just (col, op, valRaw) ->
            let typ = lookup col types
                mField = HM.lookup (B.pack col) row
                val = stripQuotes valRaw
            in fromMaybe False $ do
                field <- mField
                case typ of
                    Just "Float" -> compareNumeric (readMaybeDouble (B.unpack field)) op (readMaybeDouble val)
                    Just "Int"   -> compareNumeric (readMaybeInt (B.unpack field)) op (readMaybeInt val)
                    _            -> Just (compareText (B.unpack field) op val)
        Nothing -> False

-- Parses WHERE clause into (column, operator, value)
parseClause :: String -> Maybe (String, String, String)
parseClause str =
    let ops = [">=", "<=", "!=", "=", ">", "<"]  -- Order matters: longest first
        findOp [] = Nothing
        findOp (o:os) =
            case breakOn o str of
                Just (a, b) -> Just (trim a, o, trim b)
                Nothing     -> findOp os
    in findOp ops

-- Break string at the first occurrence of operator
breakOn :: String -> String -> Maybe (String, String)
breakOn op s =
    let (before, rest) = breakSubstring op s
    in if rest == "" then Nothing else Just (before, drop (length op) rest)

-- Break on substring
breakSubstring :: String -> String -> (String, String)
breakSubstring sub s = go "" s
  where
    go acc [] = (reverse acc, [])
    go acc rest@(x:xs)
        | sub `isPrefixOf` rest = (reverse acc, rest)
        | otherwise = go (x:acc) xs

-- Remove quotes from string
stripQuotes :: String -> String
stripQuotes ('\'':rest) = reverse (dropWhile (== '\'') (reverse rest))
stripQuotes s = s

-- Trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

-- Compare numbers
compareNumeric :: Maybe Double -> String -> Maybe Double -> Maybe Bool
compareNumeric (Just a) ">"  (Just b) = Just (a > b)
compareNumeric (Just a) "<"  (Just b) = Just (a < b)
compareNumeric (Just a) "="  (Just b) = Just (a == b)
compareNumeric (Just a) ">=" (Just b) = Just (a >= b)
compareNumeric (Just a) "<=" (Just b) = Just (a <= b)
compareNumeric (Just a) "!=" (Just b) = Just (a /= b)
compareNumeric _ _ _ = Nothing

-- Compare strings
compareText :: String -> String -> String -> Bool
compareText a "="  b = a == b
compareText a ">"  b = a > b
compareText a "<"  b = a < b
compareText a ">=" b = a >= b
compareText a "<=" b = a <= b
compareText a "!=" b = a /= b
compareText _ _ _  = False

-- Read Double
readMaybeDouble :: String -> Maybe Double
readMaybeDouble s = case reads s of [(x, "")] -> Just x; _ -> Nothing

-- Read Int and convert to Double
readMaybeInt :: String -> Maybe Double
readMaybeInt s = case reads s of [(x :: Int, "")] -> Just (fromIntegral x); _ -> Nothing

-- For ORDER BY
getValue :: String -> Row -> String
getValue col row = maybe "" B.unpack $ HM.lookup (B.pack col) row
