module QueryExecutor (runQuery) where
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import QueryParser (SQLQuery(..))
import Data.List (sortOn, isPrefixOf)

import Data.Maybe (fromMaybe)

type Row = Csv.NamedRecord

-- Run SQLQuery and get filtered rows
runQuery :: SQLQuery -> V.Vector Row -> [(String, String)] -> [Row]
runQuery query rows types =
    let filtered = maybe (V.toList rows)
                         (\clause -> filter (runWhere clause types) (V.toList rows))
                         (whereClause query)
        ordered = maybe filtered (\col -> sortOn (getValue col) filtered) (orderBy query)
        limited = maybe ordered (`take` ordered) (limitCount query)
    in selectColumns (selectCols query) (V.fromList limited)

-- Select only filtered columns
selectColumns :: [String] -> V.Vector Row -> [Row]
selectColumns cols =
    V.toList . V.map (\row -> HM.filterWithKey (\k _ -> B.unpack k `elem` cols) row)

-- Get WHERE clause into columnn/operator/value
parseClause :: String -> Maybe (String, String, String)
parseClause str =
    let ops = [">=", "<=", "!=", "=", ">", "<"]  -- important - longest first
        findOper [] = Nothing
        findOper (o:os) =
            case breakOn o str of
                Just (a, b) -> Just (trim a, o, trim b)
                Nothing     -> findOper os
    in findOper ops

-- WHERE execution based on column types
runWhere :: String -> [(String, String)] -> Row -> Bool
runWhere clause types row =
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

-- Break string at the first occurrence of operation
breakOn :: String -> String -> Maybe (String, String)
breakOn op s =
    let (before, rest) = breakSubstring op s
    in if rest == "" then Nothing else Just (before, drop (length op) rest)

-- Break on substring, used in breakOn
breakSubstring :: String -> String -> (String, String)
breakSubstring sub s = go "" s
  where
    go acc [] = (reverse acc, [])
    go acc rest@(x:xs)
        | sub `isPrefixOf` rest = (reverse acc, rest)
        | otherwise = go (x:acc) xs

-- Remove quotes from string, used in runWhere to present strings
stripQuotes :: String -> String
stripQuotes ('\'':rest) = reverse (dropWhile (== '\'') (reverse rest))
stripQuotes s = s

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

-- Comparator for numbers, use specific comparing operators from haskell
compareNumeric :: Maybe Double -> String -> Maybe Double -> Maybe Bool
compareNumeric (Just a) ">"  (Just b) = Just (a > b)
compareNumeric (Just a) "<"  (Just b) = Just (a < b)
compareNumeric (Just a) "="  (Just b) = Just (a == b)
compareNumeric (Just a) ">=" (Just b) = Just (a >= b)
compareNumeric (Just a) "<=" (Just b) = Just (a <= b)
compareNumeric (Just a) "!=" (Just b) = Just (a /= b)
compareNumeric _ _ _ = Nothing

-- Comparator for strings, same logic as for the previous one
compareText :: String -> String -> String -> Bool
compareText a "="  b = a == b
compareText a ">"  b = a > b
compareText a "<"  b = a < b
compareText a ">=" b = a >= b
compareText a "<=" b = a <= b
compareText a "!=" b = a /= b
compareText _ _ _  = False

-- Parsers of types
readMaybeDouble :: String -> Maybe Double
readMaybeDouble s = case reads s of [(x, "")] -> Just x; _ -> Nothing

readMaybeInt :: String -> Maybe Double
readMaybeInt s = case reads s of [(x :: Int, "")] -> Just (fromIntegral x); _ -> Nothing

-- Used for  order_by
getValue :: String -> Row -> String
getValue col row = maybe "" B.unpack $ HM.lookup (B.pack col) row
