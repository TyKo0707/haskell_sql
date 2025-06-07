module QueryTest (runQueryTests) where

import QueryParser
import QueryValidator
import TestFramework

runQueryTests :: IO ()
runQueryTests = do
    putStrLn "\nRunning QueryParser and QueryValidator tests..."

    -- Parser
    assertEqual "Parse: SELECT a FROM t"
        (Right $ SQLQuery ["a"] "t" Nothing Nothing Nothing)
        (parseSQLQuery "SELECT a FROM t")

    assertEqual "Parse: SELECT x,y FROM d WHERE x > 1"
        (Right $ SQLQuery ["x", "y"] "d" (Just "x > 1") Nothing Nothing)
        (parseSQLQuery "SELECT x, y FROM d WHERE x > 1")

    -- Validator
    let cols = ["x", "y", "z"]
    assertEqual "Validate known columns"
        (Right ())
        (validateQuery cols $ SQLQuery ["x", "z"] "d" Nothing Nothing Nothing)

    assertEqual "Reject unknown column"
        (Left "Unknown columns: foo")
        (validateQuery cols $ SQLQuery ["x", "foo"] "d" Nothing Nothing Nothing)
